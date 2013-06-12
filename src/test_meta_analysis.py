####################################
#                                  #
# unit tests for meta_py_r module  #
#                                  #
# Author: George Dietz             #
#         CEBM@Brown               #
#                                  #
####################################


#import unittest
#
#
#class Test_meta_py_r(unittest.TestCase):
#
#
#    def setUp(self):
#        pass
#
#
#    def tearDown(self):
#        pass
#
#
#    def testName(self):
#        pass
#
#if __name__ == "__main__":
#    #import sys;sys.argv = ['', 'Test.testName']
#    unittest.main()

#import nose
import meta_py_r

class Test_meta_analysis:
    def setup(self): # runs before each test method
        pass

    def teardown(self): # runs after each test method
        pass

    @classmethod
    def setup_class(cls): # runs before any method in the class
        pass

    @classmethod
    def teardown_class(cls): # runs after any methods in the class
        pass
    

#    def test_numbers_5_6(self):
#        print 'test_numbers_5_6()  <============================ actual test code'
#        assert multiply(5,6) == 30
#
#    def test_strings_b_2(self):
#        print 'test_strings_b_2()  <============================ actual test code'
#        assert multiply('b',2) == 'bb'


''' Desired tests:
Regular meta-analysis:
[ ] binary data * analysis methods
[ ] continuous data * analysis methods
[ ] diagnostic data * sens/spec * method (4)
                    * dor/lor   * method (3)

Cumulative meta-analysis:
[ ] binary data * analysis methods
[ ] continuous data * analysis methods
No diagnostic cum ma.

Subgroup meta-analysis: (factor covariate)
[ ] binary data * analysis method (4)
[ ] continuous data * analysis method (2)
[ ] diagnostic data * sens * method
                      spec * method
                      dor  * method
                      lor  * method


leave-one-out meta-analysis:
[ ] binary data * analysis methods
[ ] continuous data * analysis methods
[ ] diagnostic data * sens/spec * method (4)
                    * dor/lor   * method (3)

meta-regression:
[ ] binary data
    [ ] continuous covariate
    [ ] factor covariate
[ ] continuous data
    [ ] continuous covariate
    [ ] factor covariate
[ ] diagnostic data
    [ ] continuous covariate
    [ ] factor covariate
'''

def test_run_binary_ma():
    # TODO
    pass
    
def test_run_continuous_ma():
    #TODO
    pass
    
def test_run_meta_method():
    #TODO
    pass

def test_run_diagnostic_multi():
    #TODO
    pass

def test_run_meta_method_diag():
    #TODO
    pass
    
