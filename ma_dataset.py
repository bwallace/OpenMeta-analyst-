#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##  
## Custom dataset module. 
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
    
    def add_ma_unit(self, name, unit):
        self.ma_units[name] = unit
    
        
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
        self.time = time
        self.links = links
        
            
    
class Link:
    pass
    



        
    
            
        