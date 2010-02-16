#########################################################################
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
#
###########################################################################
import pdb
from PyQt4.QtCore import pyqtRemoveInputHook

 # enumeration of data types and dictionaries mapping both ways
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)
STR_TO_TYPE_DICT = {u"binary":BINARY, u"continuous":CONTINUOUS, 
                                    u"diagnostic":DIAGNOSTIC, u"OTHER":OTHER}
TYPE_TO_STR_DICT = {BINARY:u"binary", CONTINUOUS:u"continuous", 
                                    DIAGNOSTIC:u"diagnostic", OTHER:u"OTHER"}
                                    
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
        
        # @TODO
        self.covariates = []

    def get_outcome_names(self):
        if len(self) == 0:
            return []
        return sorted(self.studies[0].outcomes_to_follow_ups.keys())
        
    def get_group_names(self):
        if len(self.studies) == 0:
            return []
        all_group_names = []
        study = self.studies[0]
        for outcome in study.outcomes_to_follow_ups.keys():
            all_group_names.extend(study.outcomes_to_follow_ups[outcome][0].get_group_names())

        return list(set(all_group_names))
        
    def add_study(self, study):
        # instead, allow empty outcomes/follow-ups, but handle
        # this at the point of execution
        self.studies.append(study)
        
    def remove_study(self, id):
        self.studies = [study for study in self.studies if study.id != id]
        
    def num_studies(self):
        return len(self.studies)
            
    def get_outcome_type(self, outcome_name, get_string=False):
        outcome = self.get_outcome_obj(outcome_name)
        if outcome is None: 
            return None
        return outcome.data_type if not get_string else TYPE_TO_STR_DICT[outcome.data_type]
        
    def get_outcome_obj(self, outcome_name):
        for study in self.studies:
            outcome_obj = study.get_outcome(outcome_name)
            if outcome_obj is not None:
                return outcome_obj
        return None
        
    def add_outcome(self, outcome):
        cur_group_names = self.get_group_names()
        if len(cur_group_names) == 0:
            cur_group_names = None
            
        for study in self.studies:
            study.add_outcome(outcome, group_names=cur_group_names)
    
    def add_group(self, group_name):
        for study in self.studies:
            for outcome_name in study.outcomes_to_follow_ups.keys():
                cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                for ma_unit in cur_outcome.values():
                    ma_unit.add_group(group_name)
        print "added group: %s. cur groups: %s" % (group_name, self.get_group_names())
        
    def get_group_names(self):
        group_names = []
        for study in self.studies:
            for outcome_name in study.outcomes_to_follow_ups.keys():
                cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                for ma_unit in cur_outcome.values():
                    group_names.extend(ma_unit.get_group_names())
        return list(set(group_names))
    
    def get_network(self, outcome, time_point):
        adjacency_list = []
        for study in self.studies:
            ma_unit = study.outcomes_to_follow_ups[outcome][time_point]
            group_names = ma_unit.get_group_names()
            for g1 in group_names:
                for g2 in [group for group in group_names if group != g1]:        
                    if self.ma_unit_has_edge_between_groups(ma_unit, [g1, g2]) and\
                     not (g1, g2) in adjacency_list and not (g2, g1) in adjacency_list:
                        adjacency_list.append((g1,g2)) 

        return adjacency_list 
        
    def ma_unit_has_edge_between_groups(self, ma_unit, groups):
        # TODO this will need to be updated; right now
        # we return false if any of the groups in the unit don't
        # contain raw_data; but really we need also to check
        # for effect sizes, which may have been entered
        # independently
        for group in groups:
            if "" in ma_unit.tx_groups[group].raw_data:
                return False
        return True
        
    def cmp_studies(self, compare_by="name", reverse=True):
        if compare_by == "name":
            return lambda study_a, study_b : self._cmp_wrapper(study_a.name, study_b.name, reverse)
        elif compare_by == "year":
            return lambda study_a, study_b : self._cmp_wrapper(study_a.year, study_b.year, reverse)
    
    def _cmp_wrapper(self, study_a_val, study_b_val, reverse):
        '''
        Wraps the default compare method to assert that "" (i.e., empty studies)
        are greater than non-empties
        '''
        flip_sign = -1 if reverse else 1
        empty_vals = ("", None) # these indicate an empty row/cell
        
        if  study_a_val in empty_vals: 
            return flip_sign*1
        elif study_b_val in empty_vals:
            return flip_sign*-1
        else:
            return cmp(study_a_val, study_b_val)

        
class Study:
    '''
    This class represents a study. It basically holds a 
    list of of meta-analytic units, on which analyses can
    be performed, and some meta-data (e.g., study name)
    '''
    def __init__(self, id, name="", year=None, include=True):
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
        # whether or not this study will be included in any
        # conducted analyses
        self.include = include
        
    def add_outcome(self, outcome, group_names=None):
        ''' Adds a new, blank outcome '''
        if outcome.name in self.outcomes_to_follow_ups.keys():
            raise Exception, "Study already contains an outcome named %s" % outcome.name
        self.outcomes_to_follow_ups[outcome.name] = {}
        #group_names = list(set([outcome.group_name]))
        self.outcomes_to_follow_ups[outcome.name][0] = MetaAnalyticUnit(outcome, group_names=group_names)
        self.outcomes.append(outcome)
        
    def add_outcome_at_follow_up(self, outcome, follow_up):
        self.outcomes_to_follow_ups[outcome.name][follow_up] = MetaAnalyticUnit(outcome)
        
    def get_outcome(self, outcome_name):
        for outcome in self.outcomes:
            if outcome.name == outcome_name:
                return outcome
        return None
                
    def add_ma_unit(self, unit, time):
        if not unit.outcome in self.outcomes_to_follow_ups:
            self.add_outcome(unit.outcome)
            
        self.outcomes_to_follow_ups[unit.outcome.name][time] = unit

        
class MetaAnalyticUnit:
    '''
    This class is the unit of analysis. It corresponds to a single
    time period for a particular outcome for a dataset. Note that
    it (may) contain multiple groups!
    '''
 
    def __init__(self, outcome, raw_data = None, group_names = None):
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
        
        if group_names is None:
            group_names = ["tx A", "tx B"]
            
        self.outcome = outcome

        # TreatmentGroup ids to effect scalars.
        self.tx_groups = {}
        
        self.raw_data_length = 2 if outcome.data_type is BINARY else 3
        raw_data = raw_data or [["" for n in range(self.raw_data_length)] for group in group_names]
        # add the two default groups: treatment and control; note that the raw data
        # is held at the *group* level
        for i, group in enumerate(group_names):
            self.add_group(group)
            self.tx_groups[group].raw_data = raw_data[i]
            
        # effects_dict maps effect names -- e.g., OR, RR --
        # to dictionaries which in turn map pairwise 
        self.effects_dict = {}
        # TODO this needs another level; effect sizes that are entered directly
        # must correspond to a particular pair of tx groups, moreover the 
        # order matters i.e., the effect for tx a v. tx b is different than the reverse
        if self.outcome.data_type == BINARY:
            for effect in ["OR", "RR", "RD"]:
                self.effects_dict[effect] = {"est":None, "lower":None,
                                                             "upper":None, "variance":None}

         
    def set_effect(self, effect, value):
        self.effects_dict[effect]["est"] = value
        
    def set_effect_and_ci(self, effect, est, lower, upper):
        self.set_effect(effect, est)
        self.effects_dict[effect]["lower"] = lower
        self.effects_dict[effect]["upper"] = upper
        
    def get_effect(self, effect):
        return self.effects_dict[effect]["est"]
    
    def get_effect_and_ci(self, effect):
        return (self.effects_dict[effect]["est"], self.effects_dict[effect]["lower"], \
                    self.effects_dict[effect]["upper"])
                
    def type(self):
        return self.outcome.data_type
        
    def add_group(self, name, raw_data=None):
        if len(self.tx_groups.keys()) == 0:
            id = 0
        else:
            id = max([group.id for group in self.tx_groups.values()]) + 1
        if raw_data is None:
            raw_data = ["" for x in range(self.raw_data_length)]
        self.tx_groups[name] = TreatmentGroup(id, name, raw_data)
        
    def get_raw_data_for_group(self, group_name):
        return self.tx_groups[group_name].raw_data
        
    def get_raw_data_for_groups(self, groups):
        raw_data = []
        for group in groups:
            raw_data.extend(self.get_raw_data_for_group(group))
        return raw_data
        
    def get_group_names(self):
        return self.tx_groups.keys()
            
    
class TreatmentGroup:
    def __init__(self, id, name, raw_data):
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
    



        
    
            
        