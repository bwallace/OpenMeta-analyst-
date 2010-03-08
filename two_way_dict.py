#!/usr/bin/env python
###############################################################################
# two_way_dict python module
# Copyright (c) 2005-2008 RADLogic Pty. Ltd. 
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials provided
#      with the distribution.
#    * Neither the name of RADLogic nor the names of its contributors
#      may be used to endorse or promote products derived from this
#      software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
###############################################################################
"""Provide dictionary-style object with reverse-lookup capabilities.

Running this module as a script will run the unittest suite.

This requires Python 2.2 or greater.

"""

__author__ = 'Tim Wegener <twegener@radlogic.com.au>'
__version__ = '$Revision: 0.3 $'
__date__ = '$Date: 2008/07/31 04:05:17 $'
__credits__ = '''\
Stuart Mentzer <Stuart_Mentzer - objexx.com> - suggestions/bugfix for replace
'''

# $Log: two_way_dict.py,v $
# Revision 0.3  2008/07/31 04:05:17  twegener
# Delete old value in _reverse_map when replacing an existing key, reported by
# Stuart Mentzer <Stuart_Mentzer - objexx.com> - suggestions/bugfix for replace.
# Changed behavior of setitem when the value already exists, to be more
# symmetrical, also at Stuart's suggestion.
# Replaced some 'has_key' method calls with 'in'.
# Added todo item to replace UnitTest test suite with doctests.
#

import unittest


class TwoWayDict(dict):
    """Dictionary with reverse mapping.

    This class maintains the integrity of the two-way mapping.

    This acts just like a normal dict, but has extra methods such as:
    key(value)
    get_key(value)
    has_value(value)
    pop_key(value, [default])
    reversed_items()
    reversed_iteritems()
    reversed_popitem()

    Of these, key, get_key, has_value and reversed_items are the more commonly
    used methods.

    Notes:
    - Both keys and values must be hashable.
    - Adding a new key whose value already exists for an existing key while
      remove the existing key.
    - This takes double the storage of a normal dict.
      (Plus a little overhead.)
    - x in <TwoWayDict instance> will only return True if x is a forward key
    - Reverse lookups are implemented using a second dictionary,
      so has_value, key and get_key are O(1) operations.

    """

    def __init__(self, *args, **kwargs):

        # The user should not tamper with the internal reverse map,
        # since this enables the integrity of the TwoMayDict to be messed up.
        # Signify this with a leading underscore.
        self._reverse_map = {}

        dict.__init__(self)

        # It appears that dict.__init__ does not call self.update,
        # so need to do that here.
        self.update(*args, **kwargs)

    def __setitem__(self, k, value):
        """x.__setitem__(i, y) <==> x[i]=y"""
        
        # Check for 1-to-many mappings.
        if value in self._reverse_map:
            del self[self._reverse_map[value]]

        if k in self:
            del self._reverse_map[self[k]]

        self._reverse_map[value] = k
        
        dict.__setitem__(self, k, value)

    def __delitem__(self, k):
        """x.__delitem__(y) <==> del x[y]"""

        value = self[k]
        del self._reverse_map[value]
        dict.__delitem__(self, k)

    def __repr__(self):
        """x.__repr__() <==> repr(x)"""

        return "%s(%s)" % (self.__class__.__name__, dict.__repr__(self))

    def copy(self):
        """Return a shallow copy."""

        return self.__class__(self)

    def clear(self):
        """Remove all items."""

        dict.clear(self)
        self._reverse_map.clear()

    def key(self, value):
        """Get key corresponding to value.

        Raise KeyError if value is not present.

        """
        return self._reverse_map[value]

    def get_key(self, value, default=None):
        """Get key corresponding to value, or else a default.

        Return default if value not present.

        """
        return self._reverse_map.get(value, default)

    def has_value(self, value):
        """Return True if value is present."""

        return value in self._reverse_map

    def reversed_items(self):
        """Return a list of (value, key) pairs."""

        return self._reverse_map.items()

    def pop(self, key, *args):
        """Remove specified key and return corresponding value.

        If key is not found, default is returned if given,
        otherwise KeyError is raised.

        """
        # Don't use dict.pop, so that it works with Python 2.2.
        try:
            val = self[key]
        except KeyError:
            if not args:
                raise
            else:
                val = args[0]
        else:
            del self[key]
        return val

    def popitem(self):
        """Return and remove arbitrary (key, value) pair."""

        item = dict.popitem(self)
        del self._reverse_map[item[1]]
        return item

    def pop_key(self, value, *args):
        """Remove specified value and return corresponding key.

        If value is not found, default is returned if given,
        otherwise KeyError is raised.
        
        """
        key = self._reverse_map.pop(value, *args)
        try:
            del self[key]
        except KeyError:
            pass
        return key

    def reversed_popitem(self):
        """Return and remove arbitrary (value, key) pair."""

        item = self._reverse_map.popitem()
        del self[item[0]]
        return item

    def reversed_iteritems(self):
        """Return an iterator over the (value, key) items."""

        return self._reverse_map.iteritems()

    def update(self, dict=None, **kwargs):
        """Update from dict, or sequence of key, value pairs or kwargs."""
        
        # dict.update does not seem to call self.__getitem__,
        # so need to redefine update method here.

        # todo: Support sequence of (key, value) pairs here.
        if dict is not None:
            for k, v in dict.items():
                self[k] = v

        for k, v in kwargs.items():
            self[k] = v

    def fromkeys(cls, iterable, value=None):
        """Return new TwoWayDict with keys from iterable and values set to v.

        value defaults to None.

        This isn't very useful due to the one-to-one mapping restriction,
        but here to match Py2.3/2.4 dict interface.
        
        """
        # It is redefined here, so that it works in Py2.2.

        twd = cls()
        for key in iterable:
            twd[key] = value
        return twd
    fromkeys = classmethod(fromkeys)

    def fromvalues(cls, iterable, key=None):
        """Return new TwoWayDict with values from iterable and keys set to key.

        key defaults to None.

        This isn't very useful due to the one-to-one mapping restriction,
        but here for symmetry.
        
        """
        d = cls()
        for value in iterable:
            d[key] = value
        return d
    fromvalues = classmethod(fromvalues)


# Test suite
# todo: Replace this with doctests.
        
class TestTwoWayDictBase(unittest.TestCase):

    def setUp(self):

        # Make sure test keys are not in same sort order as values.
        self.test_keys = ['bbb', 2.0, 'c', 1, 'a']
        # Use a range for values so that order can be easily tested.
        self.test_vals = range(len(self.test_keys))
        self.test_items = zip(self.test_keys, self.test_vals)
        self.absent_key = 'absent key'
        self.absent_val = 'absent val'
        self.default_val = 'default val'
        self.new_val = 'new val'
        self.new_key = 'new key'

        self.twd = TwoWayDict()
        for k, v in self.test_items:
            self.twd[k] = v

        # Construct another ordered dict that should be equal.
        self.twd2 = TwoWayDict()
        for k, v in self.test_items:
            self.twd2[k] = v

    def test_truth(self):

        self.assert_(self.twd)
        self.assert_(not TwoWayDict())
            
    def test_keys(self):

        keys = self.twd.keys()
        keys.sort()
        test_keys_copy = self.test_keys
        test_keys_copy.sort()
        self.assertEqual(keys, test_keys_copy)

    def test_values(self):

        values = self.twd.values()
        values.sort()
        test_vals_copy = self.test_vals
        test_vals_copy.sort()
        self.assertEqual(values, test_vals_copy)

    def test_items(self):

        items = self.twd.items()
        items.sort()
        test_items_copy = self.test_items[:]
        test_items_copy.sort()
        self.assertEqual(items, test_items_copy)

    def test_reversed_items(self):

        reversed_items = self.twd.reversed_items()
        reversed_items.sort()
        reversed_test_items = []
        for k, v in self.test_items:
            reversed_test_items.append((v, k))
        reversed_test_items.sort()
        self.assertEqual(reversed_items, reversed_test_items)

    def test_getitem(self):

        for k, v in self.test_items:
            self.assertEqual(v, self.twd[k])

    def test_getitem_absent(self):

        def get_absent_key(key, self=self):

            return self.twd[key]

        self.assertRaises(KeyError, get_absent_key, self.absent_key)

    def test_get(self):

        for k, v in self.test_items:
            self.assertEqual(v, self.twd.get(k))
            
    def test_get_absent(self):

        self.assertEqual(None, self.twd.get(self.absent_key))
            
    def test_get_absent_default(self):

        self.assertEqual(self.default_val,
                         self.twd.get(self.absent_key, self.default_val))
            
    def test_get_key(self):

        for k, v in self.test_items:
            self.assertEqual(k, self.twd.get_key(v))
            
    def test_get_key_absent(self):

        self.assertEqual(None, self.twd.get_key(self.absent_val))
            
    def test_get_key_absent_default(self):

        self.assertEqual(self.default_val,
                         self.twd.get_key(self.absent_val, self.default_val))
            
    def test_len(self):

        self.assertEqual(0, len(TwoWayDict()))
        self.assertEqual(len(self.test_keys), len(self.twd))
        self.assertEqual(len(self.test_vals), len(self.twd))

    def test_has_key(self):

        for k in self.test_keys:
            self.assert_(self.twd.has_key(k))
        self.assert_(not self.twd.has_value(self.absent_key))

    def test_has_value(self):

        for v in self.test_vals:
            self.assert_(self.twd.has_value(v))
        self.assert_(not self.twd.has_value(self.absent_val))

    def test_equal(self):

        self.assertEqual(self.twd, self.twd2)

    def test_not_equal(self):

        twd_copy = self.twd.copy()
        twd_copy[self.new_key] = self.new_val
        self.assertNotEqual(self.twd, twd_copy)

    def test_copy(self):

        twd_copy = self.twd.copy()
        self.assertEqual(self.twd, twd_copy)
        self.assert_(self.twd is not twd_copy)

    def test_update(self):

        twd_other = TwoWayDict()
        twd_other.update(self.twd)
        self.assertEqual(self.twd, twd_other)
        orig_items = self.twd.items()
        orig_items.sort()
        other_items = twd_other.items()
        other_items.sort()
        orig_reversed_items = self.twd.reversed_items()
        orig_reversed_items.sort()
        other_reversed_items = twd_other.reversed_items()
        other_reversed_items.sort()
        self.assertEqual(orig_items, other_items)
        self.assertEqual(orig_reversed_items, other_reversed_items)

    def test_init_update(self):

        twd_other = TwoWayDict(self.twd)
        self.assertEqual(self.twd, twd_other)

    def test_setitem(self):

        twd_copy = self.twd.copy()
        twd_copy[self.new_key] = self.new_val
        self.assertNotEqual(self.twd, twd_copy)
        self.assertEqual(self.new_val, twd_copy[self.new_key])
        self.assert_(twd_copy.has_key(self.new_key))
        self.assert_(twd_copy.has_value(self.new_val))
        self.assert_((self.new_key, self.new_val) in twd_copy.items())

    def test_setitem_replace_key(self):

        twd_copy = self.twd.copy()
        old_key = twd_copy.key(self.test_vals[0])
        twd_copy[self.new_key] = self.test_vals[0]
        self.assert_(twd_copy.key(self.test_vals[0]) == self.new_key)
        self.assert_(old_key not in twd_copy)

    def test_delitem(self):

        twd_copy = self.twd.copy()
        del twd_copy[self.test_keys[0]]
        self.assert_(not twd_copy.has_key(self.test_keys[0]))
        self.assert_(not twd_copy.has_value(self.test_vals[0]))
        self.assertNotEqual(self.twd, twd_copy)

    def test_clear(self):

        twd_copy = self.twd.copy()
        twd_copy.clear()
        self.assertEqual(TwoWayDict(), twd_copy)
        self.assertNotEqual(self.twd, twd_copy)
        self.assert_(not len(twd_copy.reversed_items()))

    def test_setdefault(self):

        twd_copy = self.twd.copy()
        twd_copy.setdefault(self.test_keys[0])
        self.assertEqual(self.test_vals[0], twd_copy[self.test_keys[0]])

    def test_setdefault_absent(self):

        twd_copy = self.twd.copy()
        twd_copy.setdefault(self.absent_key)
        self.assertEqual(None, twd_copy[self.absent_key])

    def test_setdefault_absent_default(self):

        twd_copy = self.twd.copy()
        twd_copy.setdefault(self.absent_key, self.default_val)
        self.assertEqual(self.default_val, twd_copy[self.absent_key])

    def test_pop(self):

        twd_copy = self.twd.copy()
        v = twd_copy.pop(self.test_keys[0])
        self.assertEqual(self.test_vals[0], v)
        self.assert_(not twd_copy.has_key(self.test_keys[0]))
        self.assert_(not twd_copy.has_value(v))

    def test_pop_absent(self):

        self.assertRaises(KeyError, self.twd.pop, self.absent_key)

    def test_pop_default(self):

        twd_copy = self.twd.copy()
        v = twd_copy.pop(self.absent_key, self.default_val)
        self.assertEqual(self.default_val, v)

    def test_popitem(self):

        twd_copy = self.twd.copy()
        item = twd_copy.popitem()
        self.assert_(not twd_copy.has_key(item[0]))
        self.assert_(not twd_copy.has_value(item[1]))
        self.assert_(self.twd.has_key(item[0]))
        self.assert_(self.twd.has_value(item[1]))
        self.assertNotEqual(self.twd, twd_copy)

    def test_fromkeys_many(self):

        d = TwoWayDict.fromkeys(self.test_keys)
        self.assert_(d)
        self.assert_(self.test_keys[0] not in d)
        self.assert_(self.test_keys[-1] in d)
        
    def test_fromkeys_one(self):

        keys = [self.new_key]
        twd = TwoWayDict.fromkeys(keys)
        self.assertEqual(keys, twd.keys())
        self.assert_(twd.has_value(None))
        self.assertEqual(self.new_key, twd.get_key(None))
        self.assertEqual(None, twd[self.new_key])

    def test_reversed_iteritems(self):

        found_reversed_items = []
        for v, k in self.twd.reversed_iteritems():
            self.assertEqual(v, self.twd[k])
            self.assertEqual(k, self.twd.get_key(v))
            found_reversed_items.append((v, k))
        found_reversed_items.sort()

        reversed_items = []
        for k, v in self.test_items:
            reversed_items.append((v, k))
        reversed_items.sort()
        self.assertEqual(reversed_items, found_reversed_items)
        
    def test_iter(self):

        twd_iter = iter(self.twd)
        found_keys = []
        for k in twd_iter:
            found_keys.append(k)
        found_keys.sort()
        keys = self.test_keys[:]
        keys.sort()
        self.assertEqual(keys, found_keys)

        twd_iter = iter(self.twd)
        for k in self.test_keys:
            twd_iter.next()
        self.assertRaises(StopIteration, twd_iter.next)

    def test_iterkeys(self):

        found_keys = []
        for k in self.twd.iterkeys():
            found_keys.append(k)
        found_keys.sort()
        keys = self.test_keys[:]
        keys.sort()
        self.assertEqual(keys, found_keys)

        twd_keys_iter = self.twd.iterkeys()
        for k in self.test_keys:
            twd_keys_iter.next()

        self.assertRaises(StopIteration, twd_keys_iter.next)

    def test_itervalues(self):

        found_values = []
        for v in self.twd.itervalues():
            found_values.append(v)
        found_values.sort()
        values = self.test_vals[:]
        values.sort()
        self.assertEqual(values, found_values)

        twd_values_iter = self.twd.itervalues()
        for v in self.test_vals:
            twd_values_iter.next()
        self.assertRaises(StopIteration, twd_values_iter.next)

    def test_iteritems(self):

        found_items = []
        for item in self.twd.iteritems():
            found_items.append(item)
        found_items.sort()

        items = self.test_items[:]
        items.sort()

        self.assertEqual(items, found_items)

        twd_items_iter = self.twd.iteritems()
        for item in self.test_items:
            twd_items_iter.next()
            
        self.assertRaises(StopIteration, twd_items_iter.next)

    def test_replace(self):

        twd_copy = self.twd.copy()
        twd_copy['abcd'] = 5
        self.assert_(twd_copy.key(5) == 'abcd')

        twd_copy['abcd'] = 6
        self.assert_('abcd' in twd_copy)
        self.assert_(twd_copy.key(6) == 'abcd')
        self.assertRaises(KeyError, lambda: twd_copy.key(5))
        

if __name__ == '__main__':
    unittest.main()
