#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##  
##  Dataset module; a roll your own back end.
##
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
        return sorted(self.studies[0].outcomes_to_ma_units.keys())
        
    def add_study(self, study):
        self.studies.append(study)
        
    def remove_study(self, id):
        self.studies = [study for study in self.studies if study.id != id]
        
    def num_studies(self):
        return len(self.studies)
            
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
        
            
class MetaAnalyticUnit:
    '''
    This class is the unit of analysis. It corresponds to a single
    time period for a particular outcome for a dataset. 
    '''
    def __init__(self, outcome, is_two_group=True, raw_data = [], 
                    time=None):
        self.outcome = outcome
        self.effect_sizes = {}
        self.raw_data = raw_data
        self.time = time or 0
        
    def type(self):
        return self.outcome.data_type
        
                
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
        # of mapping time points to MetaAnalyticUnit objects
        self.outcomes_to_ma_units = {}
    
    def add_outcome(self, outcome):
        ''' Adds a new, blank outcome '''
        if outcome in self.outcomes_to_ma_units:
            raise Exception, "Study already contains an outcome named %s" % outcome.name
        self.outcomes_to_ma_units[outcome.name] = {}
        self.outcomes_to_ma_units[outcome.name][0] = MetaAnalyticUnit(outcome)
            
        
    def add_ma_unit(self, unit, time):
        if not unit.outcome in self.outcomes_to_ma_units:
            self.outcomes_to_ma_units[unit.outcome.name] = {}
        self.outcomes_to_ma_units[unit.outcome.name][time]  = unit
        
class Outcome:
    ''' Holds a few fields that define outcomes. '''
    def __init__(self, name, data_type, links=None):
        self.name = name
        self.data_type = data_type
        self.links = links
        
class Link:
    pass
    



        
    
            
        