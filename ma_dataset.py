#############################################################################
##
##  Byron C. Wallace
##  Tufts Medical Center
##  OpenMeta[analyst]
##  
##
## Custom dataset class. This is a container for study data. 
##
#############################################################################


class Dataset:
    
    # enumeration of data types
    BINARY, CONTINUOUS, DIAGNOSTIC, OTHER = range(4)
    
    # enumeration of meta-analytic types
    VANILLA, NETWORK = range(2)
    def __len__(self):
        return len(self.studies)
        
    def __init__(self, title=None, summary=None, data_type = BINARY,
                        two_group = True):
        self.title = title
        self.summary = summary
        self.studies = []
        self.num_outcomes = 0
        self.num_follow_ups = 0
        self.num_treatments = 0
        self.notes = ""
        self.data_type = data_type
        self.two_group = two_group

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
        self.ma_units = []
    
        
class MetaAnalyticUnit:
    def __init__(self):
        self.name = ""
        self.type = None
        self.effect_sizes = {}
        self.raw_data = []
        self.links = []
        
class Link:
    pass
    



        
    
            
        