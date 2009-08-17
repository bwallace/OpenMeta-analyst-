#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##  
##  Custom dataset module. 
##
#############################################################################

 # enumeration of data types
BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)

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
        self.ma_units = None

    def add_study(self, study):
        self.studies.append(study)
        
    def remove_study(self, id):
        self.studies = [study for study in self.studies if study.id != id]
        
    def num_studies(self):
        return len(self.studies)
            
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
        self.ma_units = {}
    
    def add_ma_unit(self, outcome_name, unit, time):
        if not outcome_name in self.ma_units:
            self.ma_units[outcome_name] = {}
        self.ma_units[outcome_name][time]  = unit

        
class MetaAnalyticUnit:
    '''
    This class is the unit of analysis. It corresponds to a single
    time period for a particular outcome for a dataset. 
    '''
    def __init__(self, data_type, is_two_group, raw_data = [], 
                    time=None, links = []):
        self.type = data_type
        self.effect_sizes = {}
        self.raw_data = raw_data
        self.time = time or 0
        self.links = links
        
            
    
class Link:
    pass
    



        
    
            
        