#############################################################################################
#                                                                                           #                
#  Byron C. Wallace                                                                         #  
#  Tufts Medical Center                                                                     # 
#  OpenMeta[analyst]                                                                        # 
#                                                                                           # 
#  Dataset module; a roll your own back end. This is a model for manipulating               # 
#  datasets. Note that *no QT lives here*, i.e., it is divorced from the UI entirely.       #
#                                                                                           # 
#  The structure is as follows: A Dataset object holds a list of Study objects.             # 
#  These Study objects in turn contain a dictionary, mapping outcome names                  # 
#  to another dictionary, which maps follow ups (time points) to MA_Unit                    # 
#  objects. Finally, these MA_Unit objects in turn map treatment names                      # 
#  - or groups (e.g., 'control', 'aspirin') - to raw data. Further, at the MA_Unit level,   # 
#  metrics (e.g., "OR") map to dictionaries containing that metric as computed for          # 
# the pairwise combinations of the groups/treatments (e.g., OR->"AvB"=x)                    # 
#                                                                                           # 
#############################################################################################
import pdb
from PyQt4.QtCore import pyqtRemoveInputHook
import copy

import two_way_dict
import meta_globals
from meta_globals import *

class Dataset:
    def __len__(self):
        return len(self.studies)
        
    def __init__(self, title=None, is_diag=False, summary=None):
        self.title = title
        self.summary = summary
        self.studies = []
        self.is_diag = is_diag
        self.num_outcomes = 0
        self.num_follow_ups = 0
        self.outcome_names_to_follow_ups = {}
        self.num_treatments = 0

        self.notes = ""
        
        # this will hold a list of covariate objects. each study will
        # have a dictionary with values for that study corresponding
        # to each of the covariate objects here.
        self.covariates = []

    def copy(self):
        cloned = Dataset(self.title, self.summary)
        cloned.studies = list(self.studies)
        cloned.outcome_names_to_follow_ups = copy.deepcopy(self.outcome_names_to_follow_ups)
        return cloned
        
    def get_outcome_names(self):
        return sorted(self.outcome_names_to_follow_ups.keys())
        
    def get_group_names(self):
        if len(self.studies) == 0:
            return []
        all_group_names = []
        study = self.studies[0]
        for outcome in study.outcomes_to_follow_ups.keys():
            a_follow_up = self.outcome_names_to_follow_ups[outcome].values()[0]
            all_group_names.extend(study.outcomes_to_follow_ups[outcome][a_follow_up].get_group_names())

        return list(set(all_group_names))
        
    def change_group_name(self, old_group_name, new_group_name, outcome=None, follow_up=None):
        if (outcome is None and follow_up is not None) or (follow_up is None and outcome is not None):
            raise Exception, "dataset -- change_group_name -- either both outcome and follow_up should be None, \
                                            or else neither should."
            
        for study in self.studies:
            if outcome is None and follow_up is None:
                # if no outcome/follow-up was specified, we change *all* occurences of
                # the old_group_name to the new_group_name
                for outcome_name in study.outcomes_to_follow_ups.keys():
                    cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                    for ma_unit in cur_outcome.values():                
                        ma_unit.rename_group(old_group_name, new_group_name)
            else:
                ma_unit = study.outcomes_to_follow_ups[outcome][follow_up]
                ma_unit.rename_group(old_group_name, new_group_name)
        
                            
    def change_outcome_name(self, old_outcome_name, new_outcome_name):
        self.outcome_names_to_follow_ups[new_outcome_name] = self.outcome_names_to_follow_ups.pop(old_outcome_name)
        for study in self.studies:
            study.outcomes_to_follow_ups[new_outcome_name] = study.outcomes_to_follow_ups.pop(old_outcome_name)
            for outcome in study.outcomes:
                if outcome.name == old_outcome_name:
                    outcome.name = new_outcome_name
                    break
                
    def delete_group(self, group_name):
        study = self.studies[0]
        for study in self.studies:
            for outcome_name in study.outcomes_to_follow_ups.keys():
                cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                for ma_unit in cur_outcome.values():                
                    ma_unit.remove_group(group_name)    

    def add_study(self, study, study_index=None):
        # note that we allow empty outcomes/follow-ups, but handle
        # this at the point of execution
        if study_index is None:
            self.studies.append(study)
        # the else clause was somehow removed (!!!)
        # triggering issue #91
        else:
            self.studies.insert(study_index, study)
        
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
        
    def max_study_id(self):
        if len(self.studies) == 0:
            return -1
        return max([study.id for study in self.studies])

    def remove_covariate(self, covariate):
        # first remove the covariate from the list of 
        # covariate objects for this dataset
        for cov in self.covariates:
            if cov.name == covariate.name:
                self.covariates.remove(cov)
                break
        # now remove the covariate from all of the studies
        # in the dataset
        for study in self.studies:
            if covariate.name in study.covariate_dict:
                study.covariate_dict.pop(covariate.name)
        
            
    def add_covariate(self, covariate, cov_values=None):
        ''' 
        adds the parametric covariate to: 1) the list of covariate objects
        associated with this dataset and 2) the covariate dictionaries of each
        of the studies this dataset contains. Note: the covariate argument
        needs to be a Covariate object (not a string)!
        '''
        self.covariates.append(covariate)
        if cov_values is None:
            for study in self.studies:
                study.covariate_dict[covariate.name] = None
        else:
            # in this case, a dictionary mapping studies to 
            # values for this covariate was passed in.
            # this will occur in this case, e.g., that a 
            # covariate was removed from the dataset, but then
            # the user clicked 'redo' -- we want to repopulate
            # the dataset with the previous covariate values.
            for study in self.studies:
                if cov_values.has_key(study.name):
                    study.covariate_dict[covariate.name] = cov_values[study.name]
                else:
                    study.covariate_dict[covariate.name] = None
        
            
    def get_values_for_cov(self, covariate):
        ''' returns a dictionary mapping study names to values for the given covariate '''
        cov_name = covariate
        if isinstance(covariate, Covariate):
            cov_name = covariate.name
        cov_d = {}
        for study in self.studies:  
            if study.covariate_dict.has_key(cov_name) and \
                    study.covariate_dict[cov_name] is not None:
                cov_d[study.name] = study.covariate_dict[cov_name]
        return cov_d
        
    def get_cov_names(self):
        return [cov.name for cov in self.covariates]
        
    def add_outcome(self, outcome):
        cur_group_names = self.get_group_names()
        if len(cur_group_names) == 0:
            cur_group_names = None
        
        follow_up = "first"
        self.outcome_names_to_follow_ups[outcome.name] = two_way_dict.TwoWayDict()
        self.outcome_names_to_follow_ups[outcome.name][0] = follow_up
        
        for study in self.studies:
            study.add_outcome(outcome, follow_up, group_names=cur_group_names)
    
    def remove_outcome(self, outcome_name):
        self.outcome_names_to_follow_ups.pop(outcome_name)
        for study in self.studies:
            study.remove_outcome(outcome_name)
   
    def add_group(self, group_name, outcome_name, follow_up_name=None):
        ####
        # A note on adding new groups: per consultation with the wise sir
        # Thomas Trikalinos, a decision has been made that when a 
        # group is added to an outcome, it is added by default to all
        # the follow ups belonging to said outcome. It is not, however
        # added to all the *outcomes*.
        #
        # However, if the follow_up_name argument is not None, the 
        # group will only be added to the specified follow up.
        for study in self.studies:
            cur_outcome = study.outcomes_to_follow_ups[outcome_name]
            if follow_up_name is None:
                for ma_unit in cur_outcome.values():
                    ma_unit.add_group(group_name)
            else:
                ma_unit = cur_outcome[follow_up_name]
                ma_unit.add_group(group_name)

        print "added group: %s. cur groups: %s" % (group_name, self.get_group_names())
        
    def remove_group(self, group_name):
        for study in self.studies:
            for outcome_name in study.outcomes_to_follow_ups.keys():
                cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                for ma_unit in cur_outcome.values():
                    ma_unit.remove_group(group_name)
        print "removed group: %s. cur groups: %s" % (group_name, self.get_group_names())
        
    def add_follow_up(self, follow_up_name):
        ''' adds the follow-up to *all* outcomes '''
        for outcome in self.get_outcome_names():
            self.add_follow_up_to_outcome(outcome, follow_up_name)
            
    def remove_follow_up(self, follow_up_name):
        ''' removes the follow-up from *all* outcomes '''
        for outcome in self.get_outcome_names():
            self.remove_follow_up_from_outcome(follow_up_name, outcome)
        
    def add_follow_up_to_outcome(self, outcome_name, follow_up_name):
        outcome = self.get_outcome_obj(outcome_name)
        cur_group_names = self.get_group_names()
        if len(cur_group_names) == 0:
            cur_group_names = None
        
        prev_index = max(self.outcome_names_to_follow_ups[outcome.name].keys())
        next_index = prev_index + 1

        self.outcome_names_to_follow_ups[outcome.name][next_index] = follow_up_name
        
        for study in self.studies:
            study.add_follow_up_to_outcome(outcome, follow_up_name, group_names = cur_group_names)
        
    def remove_follow_up_from_outcome(self, follow_up_name, outcome_name):
        time_point = self.outcome_names_to_follow_ups[outcome_name].get_key(follow_up_name)
            
        self.outcome_names_to_follow_ups[outcome_name].pop(time_point)
        for study in self.studies:
            study.remove_follow_up_from_outcome(outcome_name, follow_up_name)
        
    def get_group_names(self):
        group_names = []
        for study in self.studies:
            for outcome_name in study.outcomes_to_follow_ups.keys():
                cur_outcome = study.outcomes_to_follow_ups[outcome_name]
                for ma_unit in cur_outcome.values():
                    group_names.extend(ma_unit.get_group_names())
        return list(set(group_names))

    def get_group_names_for_outcome_fu(self, outcome_name, follow_up):
        group_names = []
        for study in self.studies:
            print study.name
            if study.outcomes_to_follow_ups.has_key(outcome_name):
                if study.outcomes_to_follow_ups[outcome_name].has_key(follow_up):
                    cur_ma_unit = study.outcomes_to_follow_ups[outcome_name][follow_up]
                    group_names.extend(cur_ma_unit.get_group_names())
        return list(set(group_names))
        
    def change_follow_up_name(self, outcome, old_name, new_name):
        # make sure that the follow up doesn't already exist
        if new_name in self.get_follow_up_names_for_outcome(outcome):
            raise Exception, "follow up name %s alerady exists for outcome!" % new_name
        for study in self.studies:
            study.outcomes_to_follow_ups[outcome][new_name] = study.outcomes_to_follow_ups[outcome].pop(old_name)
        # also update the outcomes -> follow-ups dictionary
        follow_up_key= self.outcome_names_to_follow_ups[outcome].get_key(old_name)
        self.outcome_names_to_follow_ups[outcome][follow_up_key] = new_name

    def get_follow_up_names(self):
        ''' returns *all* known follow-up names '''
        follow_up_names = []
        ## iterate over each outcome
        for outcome in self.outcome_names_to_follow_ups.keys():
            follow_up_names.extend(outcome.values())
        return list(set(follow_up_names))
        
    def get_study_names(self):
        return [study.name for study in self.studies]

    def get_follow_up_names_for_outcome(self, outcome):
        return self.outcome_names_to_follow_ups[outcome].values()

    def get_network(self, outcome, time_point):
        node_list = [] # list of all nodes
        adjacency_list = [] # list of edges
        for study in self.studies:
            ma_unit = study.outcomes_to_follow_ups[outcome][time_point]
            group_names = ma_unit.get_group_names()
            for g1 in group_names:
                node_list.append(g1)
                for g2 in [group for group in group_names if group != g1]:        
                    if self.ma_unit_has_edge_between_groups(ma_unit, [g1, g2]) and\
                     not (g1, g2) in adjacency_list and not (g2, g1) in adjacency_list:
                        adjacency_list.append((g1,g2)) 

        return (list(set(node_list)), adjacency_list)
        
    def ma_unit_has_edge_between_groups(self, ma_unit, groups):
        # TODO this will (probably) need to be updated; right now
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
            return lambda study_a, study_b : self._meta_cmp_wrapper(study_a, study_b,\
                                                            study_a.name, study_b.name, reverse)
        elif compare_by == "year":
            return lambda study_a, study_b : self._meta_cmp_wrapper(study_a, study_b, study_a.year, \
                                                                    study_b.year, reverse)
        else:
            # then we assume that we're sorting by a covariate
            return lambda study_a, study_b : self._meta_cmp_wrapper(study_a,\
                                                               study_b,\
                                                               study_a.covariate_dict[compare_by], \
                                                               study_b.covariate_dict[compare_by], \
                                                               reverse)
    
    def _both_empty(self, a, b):
        return a in EMPTY_VALS and b in EMPTY_VALS
        
    def _meta_cmp_wrapper(self, study_a, study_b, study_a_val, study_b_val, reverse):
        '''
        This is a bit kludgey -- we wrap the cmp wrapper in cases where the study names are not
        being compared. This is to avoid comparisons of two empty values. For example, if we are 
        sorting by a covariate, and it is empty in two studies, we want to then sort these studies by 
        their names. 
        '''
        if self._both_empty(study_a_val, study_b_val):
            # both values being compared are empty; sort by study names
            return self._cmp_wrapper(study_a.name, study_b.name, reverse)
        else:
            # at least one has a value; proceed as usual.
            return self._cmp_wrapper(study_a_val, study_b_val, reverse)
        
    def _cmp_wrapper(self, study_a_val, study_b_val, reverse):
        '''
        Wraps the default compare method to assert that "" (i.e., empty studies)
        are greater than non-empties
        '''
        flip_sign = -1 if reverse else 1
        if  study_a_val in EMPTY_VALS: 
            return flip_sign*1
        elif study_b_val in EMPTY_VALS:
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
        # TODO should fiddle with the include field here. 
        # when a study is auto-added, it should be excluded
        # until there is sufficient data
        self.id = id
        self.year = year
        self.name= name
      
        '''
        if name != "":
            self.name = name
        else:
            self.name = "study %s" % (id+1)
        '''

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
        # an empty dictionary that will map covariate names
        # to their values for *this* study.
        self.covariate_dict = {}
        self.manually_excluded = False
        
    def add_outcome(self, outcome, follow_up_name="first", group_names=None):
        ''' Adds a new, blank outcome (i.e., no raw data) '''
        if outcome.name in self.outcomes_to_follow_ups.keys():
            raise Exception, "Study already contains an outcome named %s" % outcome.name
        self.outcomes_to_follow_ups[outcome.name] = {}
        self.outcomes_to_follow_ups[outcome.name][follow_up_name] = \
                        MetaAnalyticUnit(outcome, group_names=group_names)
        self.outcomes.append(outcome)
        
    def remove_outcome(self, outcome_name):
        self.outcomes_to_follow_ups.pop(outcome_name)
        for outcome in self.outcomes:
            if outcome.name == outcome_name:
                self.outcomes.remove(outcome)
   
    def add_outcome_at_follow_up(self, outcome, follow_up):
        self.outcomes_to_follow_ups[outcome.name][follow_up] = MetaAnalyticUnit(outcome)
        
    def get_outcome(self, outcome_name):
        for outcome in self.outcomes:
            if outcome.name == outcome_name:
                return outcome
        return None
           
    def get_outcome_names(self):
        return [outcome.name for outcome in self.outcomes]
        
    def add_follow_up_to_outcome(self, outcome, follow_up_name, group_names=None):
        self.outcomes_to_follow_ups[outcome.name][follow_up_name] = \
                        MetaAnalyticUnit(outcome, group_names=group_names)
        
    def remove_follow_up_from_outcome(self, outcome, follow_up_name):
        outcome_name = outcome
        if isinstance(outcome, Outcome):
            outcome_name = outcome.name

        self.outcomes_to_follow_ups[outcome_name].pop(follow_up_name)
        
    def add_ma_unit(self, unit, follow_up):
        if not unit.outcome in self.outcomes_to_follow_ups:
            self.add_outcome(unit.outcome, follow_up)

        self.outcomes_to_follow_ups[unit.outcome.name][follow_up] = unit

        
class MetaAnalyticUnit:
    '''
    This class is the unit of analysis. It corresponds to a single
    time period for a particular outcome for a dataset. Note that
    it (may) contain multiple groups!
    '''
                    
    def __init__(self, outcome, raw_data=None, group_names=None):
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
        # diagnostic outcome?
        self.is_diag = outcome.data_type == DIAGNOSTIC
        self.outcome = outcome

        if group_names is None and not self.is_diag:
            group_names = ["tx A", "tx B"]
        elif group_names is None:
            group_names = ["test 1"]

        # TreatmentGroup ids to effect scalars. Note 
        self.tx_groups = {}
        
        self.raw_data_length = 0
        if outcome.data_type == BINARY:
            self.raw_data_length = 2
        elif outcome.data_type == CONTINUOUS:
            self.raw_data_length = 3
        else:
            self.raw_data_length = 4
        
        raw_data = raw_data or [["" for n in range(self.raw_data_length)] for group in group_names]

        self.effects_dict = {}
        
        # now we intitialize the outcome dictionaries.
        if self.outcome.data_type == BINARY:
            for effect in meta_globals.BINARY_TWO_ARM_METRICS + meta_globals.BINARY_ONE_ARM_METRICS:
                self.effects_dict[effect]={}
        elif self.outcome.data_type == CONTINUOUS:
            # note right now we only have mean difference and standardized mean difference
            # @TODO hedge's G, cohen's D, glass delta; WV doesn't implement these
            for effect in meta_globals.CONTINUOUS_TWO_ARM_METRICS + meta_globals.CONTINUOUS_ONE_ARM_METRICS:
                self.effects_dict[effect]={}                                          
        elif self.outcome.data_type == DIAGNOSTIC:
            for effect in meta_globals.DIAGNOSTIC_METRICS:
                self.effects_dict[effect]={}
                
        # add the two default groups: treatment and control; note that the raw data
        # is held at the *group* level
        for i, group in enumerate(group_names):
            self.add_group(group)
            self.tx_groups[group].raw_data = raw_data[i]
 
    def get_effect_d(self):
        # these are the dictionaries that actually hold the effects (estimate, 
        # CI, etc.). note: *always* copy this dictionary, never use it directly.
        return {"est":None, "lower":None, "upper":None, "SE":None,
                "display_est":None, "display_lower":None, "display_upper":None}    
                
    def update_effects_dict_with_group(self, new_group):
        '''
        When a new group is added, the effects dictionary will not contain
        entries for it. Thus this method must be called to update the dictionary
        with keys corresponding to this group (for one-arm metrics) and 
        keys corresponding to pairwise combinations of this with other groups.
        '''

        group_names = self.tx_groups.keys() # existing groups
        if self.outcome.data_type == BINARY:
            # we assume that an entry for each effect already exists!
            for effect in meta_globals.BINARY_TWO_ARM_METRICS:
                for group in group_names:
                    # Note that effect sizes that are entered directly
                    # must correspond to a particular *pair* of tx groups, moreover the 
                    # order matters i.e., the effect for tx a v. tx b is different than the reverse.
                    # We take care of this by mapping strings `txA-txB` to effect dictionaries
                    groups_str = "-".join((new_group, group))
                    self.effects_dict[effect][groups_str] = self.get_effect_d()
                    # ... and the reverse (see above comment)
                    groups_str = "-".join((group, new_group))
                    self.effects_dict[effect][groups_str] = self.get_effect_d()
            for effect in meta_globals.BINARY_ONE_ARM_METRICS:
                self.effects_dict[effect][new_group] = self.get_effect_d()
        elif self.outcome.data_type == CONTINUOUS:
            for effect in meta_globals.CONTINUOUS_TWO_ARM_METRICS:
                for group in group_names:
                    groups_str = "-".join((new_group, group))
                    self.effects_dict[effect][groups_str] = self.get_effect_d()
                    # and the reverse
                    groups_str = "-".join((group, new_group))
                    self.effects_dict[effect][groups_str] = self.get_effect_d()                                           
            for effect in meta_globals.CONTINUOUS_ONE_ARM_METRICS:
                self.effects_dict[effect][new_group] = self.get_effect_d()
        elif self.outcome.data_type == DIAGNOSTIC:
            # diagnostic data
            for effect in meta_globals.DIAGNOSTIC_METRICS:
                self.effects_dict[effect][new_group] = self.get_effect_d()
                    
    def set_effect(self, effect, group_str, value):
        self.effects_dict[effect][group_str]["est"] = value
       
    def set_SE(self, effect, group_str, se):
        self.effects_dict[effect][group_str]["SE"] = se
        
    def set_display_effect(self, effect, group_str, value):
        self.effects_dict[effect][group_str]["display_est"] = value
         
    def set_effect_and_ci(self, effect, group_str, est, lower, upper):
        self.set_effect(effect, group_str, est)
        self.effects_dict[effect][group_str]["lower"] = lower
        self.effects_dict[effect][group_str]["upper"] = upper
       
    def set_display_effect_and_ci(self, effect, group_str, est, lower, upper):
        self.effects_dict[effect][group_str]["display_est"] = est
        self.effects_dict[effect][group_str]["display_lower"] = lower
        self.effects_dict[effect][group_str]["display_upper"] = upper
        
    def get_effect_and_ci(self, effect, group_str):
        return (self.effects_dict[effect][group_str]["est"], self.effects_dict[effect][group_str]["lower"], \
                    self.effects_dict[effect][group_str]["upper"])
                
    def get_display_effect_and_ci(self, effect, group_str):
        return (self.effects_dict[effect][group_str]["display_est"], self.effects_dict[effect][group_str]["display_lower"], \
                    self.effects_dict[effect][group_str]["display_upper"])
        
    def set_lower(self, effect, group_str, lower):
        self.effects_dict[effect][group_str]["lower"] = lower
        
    def set_display_lower(self, effect, group_str, lower):
        self.effects_dict[effect][group_str]["display_lower"] = lower
        
    def set_upper(self, effect, group_str, upper):
        self.effects_dict[effect][group_str]["upper"] = upper
       
    def set_display_upper(self, effect, group_str, upper):
        self.effects_dict[effect][group_str]["display_upper"] = upper
         
    def get_effect(self, effect, group_str):
        return self.effects_dict[effect][group_str]["est"]
    
    def type(self):
        return self.outcome.data_type
        
    def add_group(self, name, raw_data=None):
        if len(self.tx_groups.keys()) == 0:
            id = 0
        else:
            id = max([group.id for group in self.tx_groups.values()]) + 1
        if raw_data is None:
            raw_data = ["" for x in range(self.raw_data_length)]
        # Here we add this group to the set of group keys --
        # see inline documentation in this method for details
        self.update_effects_dict_with_group(name)
        self.tx_groups[name] = TreatmentGroup(id, name, raw_data)
        
        
    def remove_group(self, name):
        self.tx_groups.pop(name)
        
    def rename_group(self, old_name, new_name):
        self.tx_groups[new_name] = self.tx_groups[old_name]
        self.tx_groups.pop(old_name)

        ##
        # also need to deal with the strings for outcome data
        # i.e., issue #112
        keys_to_pop = [] # keep track of antiquated group names to be removed
        for effect in list(self.effects_dict.keys()):
            for group_str in self.effects_dict[effect]:
                if old_name in group_str:
                    str_changed = False
                    cur_group_names = group_str.split("-")
                    cur_vals = self.effects_dict[effect][group_str]
                    updated_group_strs = []
                    for cur_group_name in cur_group_names:
                        if cur_group_name == old_name:
                            updated_group_strs.append(new_name)
                            str_changed=True
                        else:
                            updated_group_strs.append(cur_group_name)
                    
                    # if the string changed, then we pop 
                    # the old version and add the new
                    if str_changed:
                        new_str = "-".join(updated_group_strs)
                        self.effects_dict[new_str] = self.effects_dict[effect][group_str]
                          
            # now remove any antiquated group names from the effects dictionary
            for old_group_name in keys_to_pop:
                self.effects_dict[effect].pop(old_group_name)

        #pyqtRemoveInputHook()
        #pdb.set_trace()
        
    def get_raw_data_for_group(self, group_name):
        return self.tx_groups[group_name].raw_data
        
    def set_raw_data_for_group(self, group_name, raw_data):
        self.tx_groups[group_name].raw_data = raw_data
        
    def get_raw_data_for_groups(self, groups):
        if len(groups) == 1:
            return self.get_raw_data_for_group(groups[0])
        raw_data = []
        for group in groups:
            raw_data.extend(self.get_raw_data_for_group(group))
        return raw_data
        
    def set_raw_data_for_groups(self, groups, raw_data_list):
        # note: raw_data_list should be a *nested list*, where entry
        # i is the raw data for groups[i]. 
        for i,group in enumerate(groups):
            self.set_raw_data_for_group(group, raw_data_list[i])
        
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
       
class Covariate:
	''' Meta-data about covariates. '''
	def __init__(self, name, data_type):
		if not data_type in ("Factor", "Continuous"):
			raise Exception, \
				"covariates need to have associated type Factor or Continuous; %s was given" % data_type
		self.name = name
		self.data_type = CONTINUOUS if data_type == "Continuous" else FACTOR
		
	
class Link:
    pass
    



        
    
            
        