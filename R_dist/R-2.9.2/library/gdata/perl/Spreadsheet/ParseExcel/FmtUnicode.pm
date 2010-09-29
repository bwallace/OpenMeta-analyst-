# Spreadsheet::ParseExcel::FmtUnicode
#  by Kawai, Takanori (Hippo2000) 2000.12.20
#                                 2001.2.2
# This Program is ALPHA version.
#==============================================================================
package Spreadsheet::ParseExcel::FmtUnicode;
use strict;
use warnings;

use Unicode::Map;
use base 'Spreadsheet::ParseExcel::FmtDefault';

our $VERSION = '0.05';

#------------------------------------------------------------------------------
# new (for Spreadsheet::ParseExcel::FmtUnicode)
#------------------------------------------------------------------------------
sub new {
    my($sPkg, %hKey) = @_;
    my $sMap = $hKey{Unicode_Map};
    my $oMap;
    $oMap = Unicode::Map->new($sMap) if $sMap;
    my $oThis={ 
        Unicode_Map => $sMap,
        _UniMap => $oMap,
    };
    bless $oThis;
    return $oThis;
}
#------------------------------------------------------------------------------
# TextFmt (for Spreadsheet::ParseExcel::FmtUnicode)
#------------------------------------------------------------------------------
sub TextFmt {
    my($oThis, $sTxt, $sCode) =@_;
    if($oThis->{_UniMap}) {
        if(! defined($sCode)) {
            my $sSv = $sTxt;
            $sTxt =~ s/(.)/\x00$1/sg;
            $sTxt = $oThis->{_UniMap}->from_unicode($sTxt);
            $sTxt = $sSv unless($sTxt);
        }
        elsif($sCode eq 'ucs2') {
            $sTxt = $oThis->{_UniMap}->from_unicode($sTxt);
        }
#        $sTxt = $oThis->{_UniMap}->from_unicode($sTxt)
#                     if(defined($sCode) && $sCode eq 'ucs2');
        return $sTxt;
    }
    else {
        return $sTxt;
    }
}
1;
