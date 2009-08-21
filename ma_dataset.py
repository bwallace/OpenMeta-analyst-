#############################################################################
#
#  Byron C. Wallace
#  Tufts Medical Center
#  OpenMeta[analyst]
#  
#  Dataset module; a roll your own back end. This is a model for manipulating
#  datasets. Note that *no QT lives here*, i.e., it is divorced from the UI entirely.
#
#  The structure is as follows: A Dataset object holds a list of Study objects. 
#  These Study objects in turn contain a dictionary, mapping outcome names
#  to another dictionary, which maps follow ups (time points) to MA_Unit
#  objects. Finally, these MA_Unit objects in turn map treatment names
#  - or groups (e.g., 'control', 'aspirin') - to raw data. Further, at the MA_Unit level,
#  metrics (e.g., "OR") map to dictionaries containing that metric as computed for
# the pairwise combinations of the groups/treatments (e.g., OR->"AvB"=x)
#############################################################################

 # enumeration of data types
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)
STR_TO_TYPE_DICT = {u"binary":BINARY, u"continuous":CONTINUOUS, 
                                    u"diagnostic":DIAGNOSTIC, u"OTHER":OTHER}
                                    
# enumeration of meta-analytic types
VANILLA, NETWORK = range(2)
    
class Dataset:
    
    def __len__(self):
        return len(self.studies)
        
    def __init__(self, title=None, summary=None):
        self.title = title
        self.summary = summary
        self.studies = []

        
        self.num_outcomes = 0
        self.num_follow_ups = 0
        self.num_treatments = 0

        self.notes = ""

    def get_outcome_names(self):
        if len(self) == 0:
            return []
        return sorted(self.studies[0].outcomes_to_follow_ups.keys())
        
    def add_study(self, study):
        # instead, allow empty outcomes/follow-ups, but handle
        # this at the point of execution
        '''
        if len(self.studies) > 0:
            # if there are pre-existing studies, we
            # want to add all of the existing outcomes
            # for these studies to the study being added
            for outcome in self.studies[0].outcomes:
                study.add_outcome(outcome)
        '''    
        self.studies.append(study)
        
    def remove_study(self, id):
        self.studies = [study for study in self.studies if study.id != id]
        
    def num_studies(self):
        return len(self.studies)
            
    def get_outcome_type(self, outcome_name):
        if len(self.studies) == 0:
            return None
        outcome = self.studies[0].get_outcome(outcome_name)
        if outcome is None: 
            return None
        return outcome.data_type
        
    def add_outcome(self, outcome):
        for study in self.studies:
            study.add_outcome(outcome)
    
    def cmp_studies(self, compare_by="name"):
        if compare_by == "name":
            return lambda study_a, study_b : self._cmp_wrapper(study_a.name, study_b.name)
        elif compare_by == "year":
            return lambda study_a, study_b : self._cmp_wrapper(study_a.year, study_b.year)
    
    def _cmp_wrapper(self, study_a_val, study_b_val):
        '''
        Wraps the default compare method to assert that "" (i.e., empty studies)
        are greater than non-empties
        '''
        if study_a_val == "":
            return 1
        elif study_b_val == "":
            return 1
        else:
            return cmp(study_a_val, study_b_val)
        
class Study:
    '''
    This class represents a study. It basically holds a 
    list of of meta-analytic units, on which analyses can
    be performed, and some meta-data (e.g., study name)
    '''
    def __init__(self, id, name="", year=None):
        self.id = id
        self.year = year
        self.name = name
        self.N = None
        self.notes = ""
        # this dictionary maps outcome names to dictionaries
        # which in turn map follow up ids to MetaAnalyticUnit 
        # objects.
        self.outcomes_to_follow_ups = {}
        # also maintain a list of the known outcome objects
        self.outcomes = []
        
    def add_outcome(self, outcome):
        ''' Adds a new, blank outcome '''
        if outcome.name in self.outcomes_to_follow_ups.keys():
            raise Exception, "Study already contains an outcome named %s" % outcome.name
        self.outcomes_to_follow_ups[outcome.name] = {}
        self.outcomes_to_follow_ups[outcome.name][0] = MetaAnalyticUnit(outcome)
        self.outcomes.append(outcome)
        
    def get_outcome(self, outcome_name):
        for outcome in self.outcomes:
            if outcome.name == outcome_name:
                return outcome
                
    def add_ma_unit(self, unit, time):
        if not unit.outcome in self.outcomes_to_follow_ups:
            #self.outcomes_to_follow_ups[unit.outcome.name] = {}
            self.add_outcome(unit.outcome)
            
        self.outcomes_to_follow_ups[unit.outcome.name][time] = unit
        
class MetaAnalyticUnit:
    '''
    This class is the unit of analysis. It corresponds to a single
    time period for a particular outcome for a dataset. 
    '''
    def __init__(self, outcome, raw_data = [ [], [] ], group_names = ["treated", "control"]):
        '''
        Instantiate a new MetaAnalyticUnit, which is specific to a 
        given study/outcome pair. 
        
        @params:
        ===
        outcome -- Outcome object, this tells us what sort of data type
                            we have
        raw_data -- If provided, it is assumed to be a nested list, where
                            the first sublist is the raw data (num_events, num_total) 
                            for the treated group and the second corresponds to the
                            control group (if applicable)
        '''
        self.outcome = outcome
        # effects_dict maps effect names -- e.g., OR, RR --
        # to dictionaries which in turn map pairwise 
        # TreatmentGroup ids to effect scalars.
        self.tx_groups = {}
        # add the two default groups: treatment and control
        for i, group in enumerate(group_names):
            self.add_group(group)
            self.tx_groups[group].raw_data = raw_data[i]
            
        self.effects_dict = {}
        if self.outcome == BINARY:
            for effect in ["OR", "RR", "RD"]:
                self.effects_dict[effect] = {}

        
    def type(self):
        return self.outcome.data_type
        
    def add_group(self, name, raw_data=[]):
        if len(self.tx_groups.keys()) == 0:
            id = 0
        else:
            id = max([group.id for group in self.tx_groups.values()]) + 1
        self.tx_groups[name] = TreatmentGroup(id, name, raw_data)
        
    def get_raw_data_for_group(self, group_name):
        return self.tx_groups[group_name].raw_data
        
    def get_raw_data_for_groups(self, groups):
        raw_data = []
        for group in groups:
            raw_data.extend(self.get_raw_data_for_group(group))
        return raw_data
            
    
class TreatmentGroup:
    def __init__(self, id, name, raw_data=[]):
        self.id = id
        self.name = name
        self.raw_data = raw_data
    
            
class Outcome:
    ''' Holds a few fields that define outcomes. '''
    def __init__(self, name, data_type, links=None):
        self.name = name
        self.data_type = data_type
        self.links = links
        
class Link:
    pass
    



        
    
            
        