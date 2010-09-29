# Spreadsheet::ParseExcel::Utility
#  by Kawai, Takanori (Hippo2000) 2001.2.2
# This Program is ALPHA version.
#==============================================================================
# Spreadsheet::ParseExcel::Utility;
#==============================================================================
package Spreadsheet::ParseExcel::Utility;
use strict;
use warnings;

require Exporter;
use vars qw(@ISA @EXPORT_OK);
@ISA = qw(Exporter);
@EXPORT_OK = qw(ExcelFmt LocaltimeExcel ExcelLocaltime 
                col2int int2col sheetRef xls2csv);
our $VERSION = '0.06';

#my $sNUMEXP = '^[+-]?\d+(\.\d+)?$';
#my $sNUMEXP = '(^[+-]?\d+(\.\d+)?$)|(^[+-]?\d\.*(\d+)[eE][+-](\d+))$';
my $sNUMEXP = '(^[+-]?\d+(\.\d+)?$)|(^[+-]?\d+\.?(\d*)[eE][+-](\d+))$';

#------------------------------------------------------------------------------
# ExcelFmt (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub ExcelFmt {
    my($sFmt, $iData, $i1904, $sType) =@_;
    my $sCond;
    my $sWkF ='';
    my $sRes='';

# OpenOffice peculiarity?
$sFmt = '@' if ($sFmt eq "GENERAL");

#1. Get Condition
    if($sFmt=~/^\[([<>=][^\]]+)\](.*)$/) {
        $sCond = $1;
        $sFmt = $2;
    }
    $sFmt =~ s/_/ /g;

    my @sFmtWk;
    my $sFmtObj;
    my $iFmtPos=0;
    my $iDblQ=0;
    my $iQ = 0;
    foreach my $sWk (split //, $sFmt) {
        if($iDblQ or $iQ) {
            $sFmtWk[$iFmtPos] .=$sWk;
            $iDblQ = 0 if($sWk eq '"');
            $iQ = 0;
            next;
        }

        if($sWk eq ';') {
            $iFmtPos++;
            next;
        }
        elsif($sWk eq '"') {
            $iDblQ = 1;
        }
        elsif($sWk eq '!') {
            $iQ = 1;
        }
        elsif($sWk eq '\\') {
            $iQ = 1;
#            next;
        }
        elsif($sWk eq '(') { #Skip?
            next;
        }
        elsif($sWk eq ')') { #Skip?
            next;
        }
        $sFmtWk[$iFmtPos] .=$sWk;
    }
#Get FmtString
    if(scalar(@sFmtWk)>1) {
        if($sCond) {
            $sFmtObj = $sFmtWk[((eval(qq/"$iData" $sCond/))? 0: 1)];
        }
        else {
            my $iWk = ($iData =~/$sNUMEXP/)? $iData: 0;
            # $iData = abs($iData) if($iWk !=0);
            if(scalar(@sFmtWk)==2) {
                $sFmtObj = $sFmtWk[(($iWk>=0)? 0: 1)];
            }
            elsif(scalar(@sFmtWk)==3) {
                $sFmtObj = $sFmtWk[(($iWk>0)? 0: (($iWk<0)? 1:  2))];
            }
            else {
                if($iData =~/$sNUMEXP/) {
                    $sFmtObj = $sFmtWk[(($iWk>0)? 0: (($iWk<0)? 1:  2))];
                }
                else {
                    $sFmtObj = $sFmtWk[ 3];
                }
            }
        }
    }
    else {
        $sFmtObj = $sFmtWk[0];
    }

    my $sColor;
    if($sFmtObj =~ /^(\[[^hm\[\]]*\])/) {
        $sColor = $1;
        $sFmtObj = substr($sFmtObj, length($sColor));
        chop($sColor);
        $sColor = substr($sColor, 1);
    }
#print "FMT:$sFmtObj Co:$sColor\n";

#3.Build Data
    my $iFmtMode=0;   #1:Number, 2:Date
    my $i=0;
    my $ir=0;
    my $sFmtWk;
    my @aRep = ();
    my $sFmtRes='';

    my $iFflg = -1;
    my $iRpos = -1;
    my $iCmmCnt = 0;
    my $iBunFlg = 0;
    my $iFugouFlg = 0;
    my $iPer = 0;
    my $iAm=0;
    my $iSt;

    while($i<length($sFmtObj)) {
        $iSt = $i;
        my $sWk = substr($sFmtObj, $i, 1);

        if($sWk !~ /[#0\+\-\.\?eE\,\%]/) {
            if($iFflg != -1) {
                push @aRep, [substr($sFmtObj, $iFflg, $i-$iFflg),  
                                    $iRpos, $i-$iFflg];
                $iFflg= -1;
            }
        }

        if($sWk eq '"') {
            $iDblQ = $iDblQ? 0: 1;
            $i++;
            next;
        }
        elsif($sWk eq '!') {
            $iQ = 1;
            $i++;
            next;
        }
        elsif($sWk eq '\\') {
            if($iQ == 1) {
        }
        else {
                $iQ = 1;
                $i++;
                next;
            }
        }
#print "WK:", ord($sWk), " $iFmtMode \n";
#print "DEF1: $iDblQ DEF2: $iQ\n";
        if((defined($iDblQ) and ($iDblQ)) or (defined($iQ) and ($iQ))) {
            $iQ = 0;
            if(($iFmtMode != 2) and 
                ((substr($sFmtObj, $i, 2) eq "\x81\xA2") ||
                 (substr($sFmtObj, $i, 2) eq "\x81\xA3") ||
                 (substr($sFmtObj, $i, 2) eq "\xA2\xA4") ||
                 (substr($sFmtObj, $i, 2) eq "\xA2\xA5"))
                ){
#print "PUSH:", unpack("H*", substr($sFmtObj, $i, 2)), "\n";
                push @aRep, [substr($sFmtObj, $i, 2),  
                        length($sFmtRes), 2];
                $iFugouFlg = 1;
                $i+=2;
            }
            else{
                $i++;
            }
        }
        elsif(($sWk =~ /[#0\+\.\?eE\,\%]/) || 
              (($iFmtMode != 2) and 
                (($sWk eq '-') || ($sWk eq '(') || ($sWk eq ')')))
            ) {
            $iFmtMode = 1 unless($iFmtMode);
            if(substr($sFmtObj, $i, 1) =~ /[#0]/) {
                if(substr($sFmtObj, $i) =~ /^([#0]+)([\.]?)([0#]*)([eE])([\+\-])([0#]+)/){
                    push @aRep, [substr($sFmtObj, $i, length($&)), $i, length($&)];
                    $i +=length($&);
                }
                else{
                    if($iFflg==-1) {
                        $iFflg = $i;
                        $iRpos = length($sFmtRes);
                    }
                }
            }
            elsif(substr($sFmtObj, $i, 1) eq '?') {
                if($iFflg != -1) {
                    push @aRep, [substr($sFmtObj, $iFflg, $i-$iFflg+1),  
                                        $iRpos, $i-$iFflg+1];
                }
                $iFflg = $i;
                while($i<length($sFmtObj)) {
                    if (substr($sFmtObj, $i, 1) eq '/'){
                        $iBunFlg = 1;
                    }
                    elsif (substr($sFmtObj, $i, 1) eq '?'){
                        ;
                    }
                    else {
                        if(($iBunFlg) && (substr($sFmtObj, $i, 1) =~ /[0-9]/)) {
                            ;
                        }
                        else {
                            last;
                        }
                    }
                    $i++;
                }
                $i--;
                push @aRep, [substr($sFmtObj, $iFflg, $i-$iFflg+1),  
                                        length($sFmtRes), $i-$iFflg+1];
                $iFflg = -1;
            }
            elsif(substr($sFmtObj, $i, 3) =~ /^[eE][\+\-][0#]$/) {
                if(substr($sFmtObj, $i) =~ /([eE])([\+\-])([0#]+)/){
                    push @aRep, [substr($sFmtObj, $i, length($&)), $i, length($&)];
                    $i +=length($&);
                }
                $iFflg = -1;
            }
            else {
                if($iFflg != -1) {
                    push @aRep, [substr($sFmtObj, $iFflg, $i-$iFflg),  
                                        $iRpos, $i-$iFflg];
                    $iFflg= -1;
                }
                if(substr($sFmtObj, $i, 1) =~ /[\+\-]/) {
                    push @aRep, [substr($sFmtObj, $i, 1),  
                                        length($sFmtRes), 1];
                    $iFugouFlg = 1;
                }
                elsif(substr($sFmtObj, $i, 1) eq '.') {
                    push @aRep, [substr($sFmtObj, $i, 1),  
                                        length($sFmtRes), 1];
                }
                elsif(substr($sFmtObj, $i, 1) eq ',') {
                    $iCmmCnt++;
                    push @aRep, [substr($sFmtObj, $i, 1),  
                                        length($sFmtRes), 1];
                }
                elsif(substr($sFmtObj, $i, 1) eq '%') {
                    $iPer = 1;
                }
                elsif((substr($sFmtObj, $i, 1) eq '(') ||
                      (substr($sFmtObj, $i, 1) eq ')')) {
                            push @aRep, [substr($sFmtObj, $i, 1),  
                                                length($sFmtRes), 1];
                            $iFugouFlg = 1;
                }
            }
            $i++;
        }
        elsif($sWk =~ /[ymdhsapg]/) {
            $iFmtMode = 2 unless($iFmtMode);
            if(substr($sFmtObj, $i, 5) =~ /am\/pm/i) {
                push @aRep, ['am/pm', length($sFmtRes), 5];
                $iAm=1;
                $i+=5;
            }
            elsif(substr($sFmtObj, $i, 3) =~ /a\/p/i) {
                push @aRep, ['a/p', length($sFmtRes), 3];
                $iAm=1;
                $i+=3;
            }
            elsif(substr($sFmtObj, $i, 5) eq 'mmmmm') {
                push @aRep, ['mmmmm', length($sFmtRes), 5];
                $i+=5;
            }
            elsif((substr($sFmtObj, $i, 4) eq 'mmmm')  ||
                  (substr($sFmtObj, $i, 4) eq 'dddd')  ||
                  (substr($sFmtObj, $i, 4) eq 'yyyy')  ||
                  (substr($sFmtObj, $i, 4) eq 'ggge') 
                ) {
                push @aRep, [substr($sFmtObj, $i, 4), length($sFmtRes), 4];
                $i+=4;
            }
            elsif((substr($sFmtObj, $i, 3) eq 'mmm')  ||
                  (substr($sFmtObj, $i, 3) eq 'yyy')) {
                push @aRep, [substr($sFmtObj, $i, 3), length($sFmtRes), 3];
                $i+=3;
            }
            elsif((substr($sFmtObj, $i, 2) eq 'yy')  ||
              (substr($sFmtObj, $i, 2) eq 'mm')  ||
              (substr($sFmtObj, $i, 2) eq 'dd')  ||
              (substr($sFmtObj, $i, 2) eq 'hh')  ||
              (substr($sFmtObj, $i, 2) eq 'ss')  ||
              (substr($sFmtObj, $i, 2) eq 'ge')) {
                if((substr($sFmtObj, $i, 2) eq 'mm') &&
                   ($#aRep>=0) && 
                    (($aRep[$#aRep]->[0] eq 'h') or ($aRep[$#aRep]->[0] eq 'hh'))) {
                        push @aRep, ['mm', length($sFmtRes), 2, 'min'];
                }
                else {
                        push @aRep, [substr($sFmtObj, $i, 2), length($sFmtRes), 2];
                }
                if((substr($sFmtObj, $i, 2) eq 'ss') && ($#aRep>0)) {
                    if(($aRep[$#aRep-1]->[0] eq 'm') ||
                       ($aRep[$#aRep-1]->[0] eq 'mm')) {
                        push(@{$aRep[$#aRep-1]}, 'min');
                    }
                }
                $i+=2;
            }
            elsif((substr($sFmtObj, $i, 1) eq 'm')  ||
                  (substr($sFmtObj, $i, 1) eq 'd')  ||
                  (substr($sFmtObj, $i, 1) eq 'h')  ||
                  (substr($sFmtObj, $i, 1) eq 's')){
                if((substr($sFmtObj, $i, 1) eq 'm') &&
                   ($#aRep>=0) && 
                    (($aRep[$#aRep]->[0] eq 'h') or ($aRep[$#aRep]->[0]  eq 'hh'))) {
                        push @aRep, ['m', length($sFmtRes), 1, 'min'];
                }
                else {
                        push @aRep, [substr($sFmtObj, $i, 1), length($sFmtRes), 1];
                }
                if((substr($sFmtObj, $i, 1) eq 's') && ($#aRep>0)) {
                    if(($aRep[$#aRep-1]->[0] eq 'm') ||
                       ($aRep[$#aRep-1]->[0] eq 'mm')) {
                        push(@{$aRep[$#aRep-1]}, 'min');
                    }
                }
                $i+=1;
            }
        }
        elsif((substr($sFmtObj, $i, 3) eq '[h]')) {
            push @aRep, ['[h]', length($sFmtRes), 3];
            $i+=3;
        }
        elsif((substr($sFmtObj, $i, 4) eq '[mm]')) {
            push @aRep, ['[mm]', length($sFmtRes), 4];
            $i+=4;
        }
        elsif($sWk eq '@') {
            push @aRep, ['@', length($sFmtRes), 1];
            $i++;
        }
        elsif($sWk eq '*') {
            push @aRep, [substr($sFmtObj, $i, 1),  
                                        length($sFmtRes), 1];
        }
        else{
            $i++;
        }
        $i++ if($i == $iSt);        #No Format match
        $sFmtRes .= substr($sFmtObj, $iSt, $i-$iSt);
    }
#print "FMT: $iRpos ",$sFmtRes, "\n";
    if($iFflg != -1) {
        push @aRep, [substr($sFmtObj, $iFflg, $i-$iFflg+1),
                    $iRpos,, $i-$iFflg+1];
        $iFflg= 0;
    }

#For Date format
    $iFmtMode = 0 if(defined $sType && $sType eq 'Text');   #Not Convert Non Numeric
    if(($iFmtMode==2)&& ($iData =~/$sNUMEXP/)) {
        my @aTime = ExcelLocaltime($iData, $i1904);
        $aTime[4]++;
        $aTime[5] += 1900;

        my @aMonL = 
            qw (dum January February March April May June July 
                August September October November December );
        my @aMonNm =
            qw (dum Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
        my @aWeekNm = 
            qw (Mon Tue Wed Thu Fri Sat Sun);
        my @aWeekL = 
            qw (Monday Tuesday Wednesday Thursday Friday Saturday Sunday);
        my $sRep;
        for(my $iIt=$#aRep; $iIt>=0;$iIt--) {
            my $rItem = $aRep[$iIt];
            if((scalar @$rItem) >=4) {
    #Min
                if($rItem->[0] eq 'mm') {
                    $sRep = sprintf("%02d", $aTime[1]);
                }
                else {
                    $sRep = sprintf("%d", $aTime[1]);
                }
            }
    #Year
            elsif($rItem->[0] eq 'yyyy') {
                $sRep = sprintf('%04d', $aTime[5]);
            }
            elsif($rItem->[0] eq 'yy') {
                $sRep = sprintf('%02d', $aTime[5] % 100);
            }
    #Mon
            elsif($rItem->[0] eq 'mmmmm') {
                $sRep = substr($aMonNm[$aTime[4]], 0, 1);
            }
            elsif($rItem->[0] eq 'mmmm') {
                $sRep = $aMonL[$aTime[4]];
            }
            elsif($rItem->[0] eq 'mmm') {
                $sRep = $aMonNm[$aTime[4]];
            }
            elsif($rItem->[0] eq 'mm') {
                $sRep = sprintf('%02d', $aTime[4]);
            }
            elsif($rItem->[0] eq 'm') {
                $sRep = sprintf('%d', $aTime[4]);
            }
    #Day
            elsif($rItem->[0] eq 'dddd') {
                $sRep = $aWeekL[$aTime[7]];
            }
            elsif($rItem->[0] eq 'ddd') {
                $sRep = $aWeekNm[$aTime[7]];
            }
            elsif($rItem->[0] eq 'dd') {
                $sRep = sprintf('%02d', $aTime[3]);
            }
            elsif($rItem->[0] eq 'd') {
                $sRep = sprintf('%d', $aTime[3]);
            }
    #Hour
            elsif($rItem->[0] eq 'hh') {
                if($iAm) {
                    $sRep = sprintf('%02d', $aTime[2]%12);
                }
                else {
                    $sRep = sprintf('%02d', $aTime[2]);
                }
            }
            elsif($rItem->[0] eq 'h') {
                if($iAm) {
                    $sRep = sprintf('%d', $aTime[2]%12);
                }
                else {
                    $sRep = sprintf('%d', $aTime[2]);
                }
            }
    #SS
            elsif($rItem->[0] eq 'ss') {
                $sRep = sprintf('%02d', $aTime[0]);
            }
            elsif($rItem->[0] eq 'S') {
                $sRep = sprintf('%d', $aTime[0]);
            }
    #am/pm
            elsif($rItem->[0] eq 'am/pm') {
                $sRep = ($aTime[4]>12)? 'pm':'am';
            }
            elsif($rItem->[0] eq 'a/p') {
                $sRep = ($aTime[4]>12)? 'p':'a';
            }
            elsif($rItem->[0] eq '.') {
                $sRep = '.';
            }
            elsif($rItem->[0] =~ /^0+$/) {
                my $i0Len = length($&);
#print "SEC:", $aTime[7], "\n";
                $sRep = substr(sprintf("%.${i0Len}f", $aTime[7]/1000.0), 2, $i0Len);
            }
            elsif($rItem->[0] eq '[h]') {
                $sRep = sprintf('%d', int($iData) * 24 + $aTime[2]);
            }
            elsif($rItem->[0] eq '[mm]') {
                $sRep = sprintf('%d', (int($iData) * 24 + $aTime[2])*60 + $aTime[1]);
            }
#NENGO(Japanese)
            elsif($rItem->[0] eq 'ge') {
                $sRep = Spreadsheet::ParseExcel::FmtJapan::CnvNengo(1, @aTime);
            }
            elsif($rItem->[0] eq 'ggge') {
                $sRep = Spreadsheet::ParseExcel::FmtJapan::CnvNengo(2, @aTime);
            }
            elsif($rItem->[0] eq '@') {
                $sRep = $iData;
            }

#print "REP:$sRep ",$rItem->[0], ":", $rItem->[1], ":" ,$rItem->[2], "\n";
            substr($sFmtRes, $rItem->[1], $rItem->[2]) = $sRep;
        }
    }
    elsif(($iFmtMode==1)&& ($iData =~/$sNUMEXP/)) {
        if($#aRep>=0) {
            while($aRep[$#aRep]->[0] eq ',') {
                $iCmmCnt--;
                substr($sFmtRes, $aRep[$#aRep]->[1], $aRep[$#aRep]->[2]) = '';
                $iData /= 1000;
                pop @aRep;
            }

            my $sNumFmt = join('', map {$_->[0]} @aRep);
            my $sNumRes;
            my $iTtl=0;
            my $iE=0;
            my $iP=0;
            my $iInt = 0;
            my $iAftP=undef;
            foreach my $sItem (split //, $sNumFmt) {
                if($sItem eq '.') {
                    $iTtl++;
                    $iP = 1;
                }
                elsif(($sItem eq 'E') || ($sItem eq 'e')){
                    $iE = 1;
                }
                elsif($sItem eq '0') {
                    $iTtl++;
                    $iAftP++ if($iP);
                    $iInt = 1;
                }
                elsif($sItem eq '#') {
                    #$iTtl++;
                    $iAftP++ if($iP);
                    $iInt = 1;
                }
                elsif($sItem eq '?') {
                    #$iTtl++;
                    $iAftP++ if($iP);
                }
            }
#print "DATA:$iData\n";
            $iData *= 100.0 if($iPer);
            my $iDData = ($iFugouFlg)? abs($iData) : $iData+0;
            if($iBunFlg) {
                $sNumRes = sprintf("%0${iTtl}d", int($iDData));
            }
            else {
                if($iP) {
#                    $sNumRes = sprintf("%0${iTtl}.${iAftP}f", $iDData);
                    $sNumRes = sprintf(
                         (defined($iAftP)? 
                         "%0${iTtl}.${iAftP}f": "%0${iTtl}f"), $iDData);
                }
                else {
    #print "DATA:", $iDData, "\n";
                    $sNumRes = sprintf("%0${iTtl}.0f", $iDData);
                }
            }
#print "sNum:$sNumRes\n";
            $sNumRes = AddComma($sNumRes) if($iCmmCnt > 0);
#print "RES:$sNumRes\n";
            my $iLen = length($sNumRes);
            my $iPPos = -1;
            my $sRep;

            for(my $iIt=$#aRep; $iIt>=0;$iIt--) {
                my $rItem = $aRep[$iIt];
#print "Rep:", unpack("H*", $rItem->[0]), "\n";
                if($rItem->[0] =~/([#0]*)([\.]?)([0#]*)([eE])([\+\-])([0#]+)/) {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = 
                            MakeE($rItem->[0], $iData);
                }
                elsif($rItem->[0] =~ /\//) {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = 
                        MakeBun($rItem->[0], $iData, $iInt);
                }
                elsif($rItem->[0] eq '.') {
                    $iLen--;
                    $iPPos=$iLen;
                }
                elsif($rItem->[0] eq '+') {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = 
                        ($iData > 0)? '+': (($iData==0)? '+':'-');
                }
                elsif($rItem->[0] eq '-') {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = 
                        ($iData > 0)? '': (($iData==0)? '':'-');
                }
                elsif($rItem->[0] eq '@') {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = $iData;
                }
                elsif($rItem->[0] eq '*') {
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = ''; #REMOVE
                }
                elsif(($rItem->[0] eq "\xA2\xA4") or ($rItem->[0] eq "\xA2\xA5") or
                      ($rItem->[0] eq "\x81\xA2") or ($rItem->[0] eq "\x81\xA3") ){
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = $rItem->[0];
                            # ($iData > 0)? '': (($iData==0)? '':$rItem->[0]);
                }
                elsif(($rItem->[0] eq '(') or ($rItem->[0] eq ')')){
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = $rItem->[0];
                         # ($iData > 0)? '': (($iData==0)? '':$rItem->[0]);
                }
                else {
                    if($iLen>0) {
                        if($iIt <= 0) {
                            $sRep = substr($sNumRes, 0, $iLen);
                            $iLen = 0;
                        }
                        else {
                            my $iReal = length($rItem->[0]);
                            if($iPPos >= 0) {
                                my $sWkF = $rItem->[0];
                                $sWkF=~s/^#+//;
                                $iReal = length($sWkF);
                                $iReal = ($iLen <=$iReal)? $iLen:$iReal;
                            }
                            else {
                                $iReal = ($iLen <=$iReal)? $iLen:$iReal;
                            }
                            $sRep = substr($sNumRes, $iLen - $iReal, $iReal);
                            $iLen -=$iReal;
                        }
                    }
                    else {
                            $sRep = '';
                    }
                    substr($sFmtRes, $rItem->[1], $rItem->[2]) = "\x00" . $sRep;
                }
            }
            $sRep = ($iLen > 0)?substr($sNumRes, 0, $iLen) : '';
            $sFmtRes =~ s/\x00/$sRep/;
            $sFmtRes =~ s/\x00//g;
        }
    }
    else {
       my $iAtMk = 0;
        for(my $iIt=$#aRep; $iIt>=0;$iIt--) {
            my $rItem = $aRep[$iIt];
            if($rItem->[0] eq '@') {
                substr($sFmtRes, $rItem->[1], $rItem->[2]) = $iData;
                $iAtMk++;
            }
            else {
                substr($sFmtRes, $rItem->[1], $rItem->[2]) = '';
            }
        }
        $sFmtRes = $iData unless($iAtMk);
    }
    return wantarray()? ($sFmtRes, $sColor) : $sFmtRes;
}
#------------------------------------------------------------------------------
# AddComma (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub AddComma {
    my($sNum) = @_;

    if($sNum=~ /^([^\d]*)(\d\d\d\d+)(\.*.*)$/) {
        my($sPre, $sObj, $sAft) =($1, $2, $3);
        for(my $i=length($sObj)-3;$i>0; $i-=3) {
            substr($sObj, $i, 0) = ',';
        }
        return $sPre . $sObj . $sAft;
    }
    else {
        return $sNum;
    }
}
#------------------------------------------------------------------------------
# MakeBun (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub MakeBun {
    my($sFmt, $iData, $iFlg) = @_;
    my $iBunbo;
    my $iShou;

#1. Init
#print "FLG: $iFlg\n";
    if($iFlg) {
        $iShou = $iData - int($iData);
        return '' if($iShou == 0);
    }
    else {
        $iShou = $iData;
    }
    $iShou = abs($iShou);
    my $sSWk;

#2.Calc BUNBO
#2.1 BUNBO defined
    if($sFmt =~ /\/(\d+)$/) {
        $iBunbo = $1;
        return sprintf("%d/%d", $iShou*$iBunbo, $iBunbo);
    }
    else {
#2.2 Calc BUNBO
        $sFmt =~ /\/(\?+)$/;
        my $iKeta = length($1);
        my $iSWk = 1;
        my $sSWk = '';
        my $iBunsi;
        for(my $iBunbo = 2;$iBunbo<10**$iKeta;$iBunbo++) {
            $iBunsi = int($iShou*$iBunbo + 0.5);
            my $iCmp = abs($iShou - ($iBunsi/$iBunbo));
            if($iCmp < $iSWk) {
                $iSWk =$iCmp;
                $sSWk = sprintf("%d/%d", $iBunsi, $iBunbo);
                last if($iSWk==0);
            }
        }
        return $sSWk;
    }
}
#------------------------------------------------------------------------------
# MakeE (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub MakeE {
    my($sFmt, $iData) = @_;

    $sFmt=~/(([#0]*)[\.]?[#0]*)([eE])([\+\-][0#]+)/;
    my($sKari, $iKeta, $sE, $sSisu) = ($1, length($2), $3, $4);
    $iKeta = 1 if($iKeta<=0);

    my $iLog10 = 0;
    $iLog10 = ($iData == 0)? 0 : (log(abs($iData))/ log(10));
    $iLog10 = (int($iLog10 / $iKeta) + 
            ((($iLog10 - int($iLog10 / $iKeta))<0)? -1: 0)) *$iKeta;

    my $sUe = ExcelFmt($sKari, $iData*(10**($iLog10*-1)),0);
    my $sShita = ExcelFmt($sSisu, $iLog10, 0);
    return $sUe . $sE . $sShita;
}
#------------------------------------------------------------------------------
# LeapYear (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub LeapYear {
    my($iYear)=@_;
    return 1 if($iYear==1900); #Special for Excel
    return ((($iYear % 4)==0) && (($iYear % 100) || ($iYear % 400)==0))? 1: 0;
}
#------------------------------------------------------------------------------
# LocaltimeExcel (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub LocaltimeExcel {
    my($iSec, $iMin, $iHour, $iDay, $iMon, $iYear, $iMSec, $flg1904) = @_;

#0. Init
    $iMon++;
    $iYear+=1900;

#1. Calc Time
    my $iTime;
    $iTime =$iHour;
    $iTime *=60;
    $iTime +=$iMin;
    $iTime *=60;
    $iTime +=$iSec;
    $iTime += $iMSec/1000.0 if(defined($iMSec)) ;
    $iTime /= 86400.0;      #3600*24(1day in seconds)
    my $iY;
    my $iYDays;

#2. Calc Days
    if($flg1904) {
        $iY = 1904;
        $iTime--;         #Start from Jan 1st
        $iYDays = 366;
    }
    else {
        $iY = 1900;
        $iYDays = 366;  #In Excel 1900 is leap year (That's not TRUE!)
    }
    while($iY<$iYear) {
        $iTime += $iYDays;
        $iY++;
        $iYDays = (LeapYear($iY))? 366: 365;
    }
    for(my $iM=1;$iM < $iMon; $iM++){
        if($iM == 1 || $iM == 3 || $iM == 5 || $iM == 7 || $iM == 8
            || $iM == 10 || $iM == 12) {
            $iTime += 31;
        }
        elsif($iM == 4 || $iM == 6 || $iM == 9 || $iM == 11) {
            $iTime += 30;
        }
        elsif($iM == 2) {
            $iTime += (LeapYear($iYear))? 29: 28;
        }
    }
    $iTime+=$iDay;
    return $iTime;
}
#------------------------------------------------------------------------------
# ExcelLocaltime (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
sub ExcelLocaltime {
  my($dObj, $flg1904) = @_;
  my($iSec, $iMin, $iHour, $iDay, $iMon, $iYear, $iwDay, $iMSec);
  my($iDt, $iTime, $iYDays);

  $iDt  = int($dObj);
  $iTime = $dObj - $iDt;

#1. Calc Days
  if($flg1904) {
    $iYear = 1904;
    $iDt++;         #Start from Jan 1st
    $iYDays = 366;
    $iwDay = (($iDt+4) % 7);
  }
  else {
    $iYear = 1900;
    $iYDays = 366;  #In Excel 1900 is leap year (That's not TRUE!)
    $iwDay = (($iDt+6) % 7);
  }
  while($iDt > $iYDays) {
    $iDt -= $iYDays;
    $iYear++;
    $iYDays = ((($iYear % 4)==0) && 
        (($iYear % 100) || ($iYear % 400)==0))? 366: 365;
  }
  $iYear -= 1900;
  for($iMon=1;$iMon < 12; $iMon++){
    my $iMD;
    if($iMon == 1 || $iMon == 3 || $iMon == 5 || $iMon == 7 || $iMon == 8
        || $iMon == 10 || $iMon == 12) {
        $iMD = 31;
    }
    elsif($iMon == 4 || $iMon == 6 || $iMon == 9 || $iMon == 11) {
        $iMD = 30;
    }
    elsif($iMon == 2) {
        $iMD = (($iYear % 4) == 0)? 29: 28;
    }
    last if($iDt <= $iMD);
    $iDt -= $iMD;
  }

#2. Calc Time
  $iDay = $iDt;
  $iTime += (0.0005 / 86400.0);
  $iTime*=24.0;
  $iHour = int($iTime);
  $iTime -= $iHour;
  $iTime *= 60.0;
  $iMin  = int($iTime);
  $iTime -= $iMin;
  $iTime *= 60.0;
  $iSec  = int($iTime);
  $iTime -= $iSec;
  $iTime *= 1000.0;
  $iMSec = int($iTime);
    
  return ($iSec, $iMin, $iHour, $iDay, $iMon-1, $iYear, $iwDay, $iMSec);
}
# -----------------------------------------------------------------------------
# col2int (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
# converts a excel row letter into an int for use in an array
sub col2int {
    my $result = 0 ;
    my $str = shift ;
   my $incr = 0 ;

    for(my $i = length($str) ; $i > 0 ; $i--) {
        my $char = substr( $str, $i-1) ;
        my $curr += ord(lc($char)) - ord('a') + 1;
        $curr *= $incr if( $incr) ;
        $result += $curr ;
        $incr += 26 ;
    }
    # this is one out as we range 0..x-1 not 1..x
    $result-- ;

    return $result ;
}
# -----------------------------------------------------------------------------
# int2col (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
### int2col
# convert a column number into column letters
# @note this is quite a brute force coarse method
#   does not manage values over 701 (ZZ)
# @arg number, to convert
# @returns string, column name
#
sub int2col {
   my $out = "" ;
   my $val = shift ;
   
   do {
      $out .= chr(( $val % 26) + ord('A')) ;
      $val = int( $val / 26) - 1 ;
   } while( $val >= 0) ;
   
   return reverse $out ;
}
# -----------------------------------------------------------------------------
# sheetRef (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
# -----------------------------------------------------------------------------
### sheetRef
# convert an excel letter-number address into a useful array address
# @note that also Excel uses X-Y notation, we normally use Y-X in arrays
# @args $str, excel coord eg. A2
# @returns an array - 2 elements - column, row, or undefined
#
sub sheetRef {
    my $str = shift ;
    my @ret ;

    $str =~ m/^(\D+)(\d+)$/ ;

    if( $1 && $2) {
        push( @ret, $2 -1, col2int($1)) ;
    }
    if( $ret[0] < 0) {
        undef @ret ;
    }

    return @ret ;
}
# -----------------------------------------------------------------------------
# xls2csv (for Spreadsheet::ParseExcel::Utility)
#------------------------------------------------------------------------------
### xls2csv
# convert a chunk of an excel file into csv text chunk
# @args $param, sheet-colrow:colrow (1-A1:B2 or A1:B2 for sheet 1
# @args $rotate, 0 or 1 decides if output should be rotated or not
# @returns string containing a chunk of csv
#
sub xls2csv {
   my ($filename, $regions, $rotate) = @_ ;
   my $sheet = 0 ;
   my $output = "" ;   
   
   # extract any sheet number from the region string
   $regions =~ m/^(\d+)-(.*)/ ;

   if( $2) {
      $sheet = $1 - 1 ;
      $regions = $2 ;
    }

    # now extract the start and end regions
    $regions =~ m/(.*):(.*)/ ;

    if( !$1 || !$2) {
        print STDERR "Bad Params";
        return "" ;
    }

   my @start = sheetRef( $1) ;
   my @end = sheetRef( $2) ;
   if( !@start) {
        print STDERR "Bad coorinates - $1";
        return "" ;
   }
   if( !@end) {
        print STDERR "Bad coorinates - $2";
        return "" ;
   }
   
   if( $start[1] > $end[1]) {
        print STDERR "Bad COLUMN ordering\n";
        print STDERR "Start column " . int2col($start[1]);
        print STDERR " after end column " . int2col($end[1]) . "\n";
        return "" ;
   }
   if( $start[0] > $end[0]) {
        print STDERR "Bad ROW ordering\n";
        print STDERR "Start row " . ($start[0] + 1);
        print STDERR " after end row " . ($end[0] + 1) . "\n";
        exit ;
   }
   
   # start the excel object now
   my $oExcel = new Spreadsheet::ParseExcel ;
   my $oBook = $oExcel->Parse( $filename) ;
   # open the sheet
   my $oWkS = $oBook->{Worksheet}[ $sheet] ;

   # now check that the region exists in the file
   # if not trucate to the possible region
   # output a warning msg
   if( $start[1] < $oWkS->{MinCol}) {
        print STDERR int2col( $start[1]) . " < min col " . int2col( $oWkS->{MinCol}) . " Reseting\n";
        $start[1] = $oWkS->{MinCol} ;
   }
   if( $end[1] > $oWkS->{MaxCol}) {
        print STDERR int2col( $end[1]) . " > max col " . int2col( $oWkS->{MaxCol}) . " Reseting\n" ;
        $end[1] = $oWkS->{MaxCol} ;
   }
   if( $start[0] < $oWkS->{MinRow}) {
        print STDERR "" . ($start[0] + 1) . " < min row " . ($oWkS->{MinRow} + 1) . " Reseting\n";
        $start[0] = $oWkS->{MinCol} ;
   }
   if( $end[0] > $oWkS->{MaxRow}) {
        print STDERR "" . ($end[0] + 1) . " > max row " . ($oWkS->{MaxRow} + 1) . " Reseting\n";
        $end[0] = $oWkS->{MaxRow} ;
   
   }

   my $x1 = $start[1] ;
   my $y1 = $start[0] ;
   my $x2 = $end[1] ;
   my $y2 = $end[0] ;

    if( !$rotate) {       
    for( my $y = $y1 ; $y <= $y2 ; $y++) {
       for( my $x = $x1 ; $x <= $x2 ; $x++) {
          my $cell = $oWkS->{Cells}[$y][$x] ;
          $output .=  $cell->Value if(defined $cell);
          $output .= "," if( $x != $x2) ;
       }
       $output .= "\n" ;
    }
     } else {
    for( my $x = $x1 ; $x <= $x2 ; $x++) {
       for( my $y = $y1 ; $y <= $y2 ; $y++) {
          my $cell = $oWkS->{Cells}[$y][$x] ;
          $output .=  $cell->Value if(defined $cell);
          $output .= "," if( $y != $y2) ;
       }
       $output .= "\n" ;
    }
     }
   
   return $output ;
}

1;
__END__

=head1 NAME

Spreadsheet::ParseExcel::Utility - Utility function for Spreadsheet::ParseExcel

=head1 SYNOPSIS

    use strict;
    #Declare
    use Spreadsheet::ParseExcel::Utility qw(ExcelFmt ExcelLocaltime LocaltimeExcel);
    
    #Convert localtime ->Excel Time
    my $iBirth = LocaltimeExcel(11, 10, 12, 23, 2, 64);
                               # = 1964-3-23 12:10:11
    print $iBirth, "\n";       # 23459.5070717593
    
    #Convert Excel Time -> localtime
    my @aBirth = ExcelLocaltime($iBirth, undef);
    print join(":", @aBirth), "\n";   # 11:10:12:23:2:64:1:0
    
    #Formatting
    print ExcelFmt('yyyy-mm-dd', $iBirth), "\n"; #1964-3-23
    print ExcelFmt('m-d-yy', $iBirth), "\n";     # 3-23-64
    print ExcelFmt('#,##0', $iBirth), "\n";      # 23,460
    print ExcelFmt('#,##0.00', $iBirth), "\n";   # 23,459.51
    print ExcelFmt('"My Birthday is (m/d):" m/d', $iBirth), "\n";
                                      # My Birthday is (m/d): 3/23

=head1 DESCRIPTION

Spreadsheet::ParseExcel::Utility exports utility functions concerned with Excel format setting.

=head1 Functions

This module can export 3 functions: ExcelFmt, ExcelLocaltime and LocaltimeExcel.

=head2 ExcelFmt

$sTxt = ExcelFmt($sFmt, $iData [, $i1904]);

I<$sFmt> is a format string for Excel. I<$iData> is the target value.
If I<$flg1904> is true, this functions assumes that epoch is 1904.
I<$sTxt> is the result.

For more detail and examples, please refer sample/chkFmt.pl in this distribution.

ex.
  
=head2 ExcelLocaltime

($iSec, $iMin, $iHour, $iDay, $iMon, $iYear, $iwDay, $iMSec) = 
            ExcelLocaltime($iExTime [, $flg1904]);

I<ExcelLocaltime> converts time information in Excel format into Perl localtime format.
I<$iExTime> is a time of Excel. If I<$flg1904> is true, this functions assumes that
epoch is 1904.
I<$iSec>, I<$iMin>, I<$iHour>, I<$iDay>, I<$iMon>, I<$iYear>, I<$iwDay> are same as localtime.
I<$iMSec> means 1/1,000,000 seconds(ms).


=head2 LocaltimeExcel

I<$iExTime> = LocaltimeExcel($iSec, $iMin, $iHour, $iDay, $iMon, $iYear [,$iMSec] [,$flg1904])

I<LocaltimeExcel> converts time information in Perl localtime format into Excel format .
I<$iSec>, I<$iMin>, I<$iHour>, I<$iDay>, I<$iMon>, I<$iYear> are same as localtime.

If I<$flg1904> is true, this functions assumes that epoch is 1904.
I<$iExTime> is a time of Excel. 

=head2 col2int

I<$iInt> = col2int($sCol);

converts a excel row letter into an int for use in an array

This function was contributed by Kevin Mulholland.

=head2 int2col

I<$sCol> = int2col($iRow);

convert a column number into column letters
NOET: This is quite a brute force coarse method does not manage values over 701 (ZZ)

This function was contributed by Kevin Mulholland.

=head2 sheetRef

(I<$iRow>, I<$iCol>) = sheetRef($sStr);

convert an excel letter-number address into a useful array address
NOTE: That also Excel uses X-Y notation, we normally use Y-X in arrays
$sStr, excel coord (eg. A2).

This function was contributed by Kevin Mulholland.

=head2 xls2csv

$sCsvTxt = xls2csv($sFileName, $sRegion, $iRotate);

convert a chunk of an excel file into csv text chunk
$sRegions = "sheet-colrow:colrow" (ex. '1-A1:B2' means 'A1:B2' for sheet 1)
$iRotate  = 0 or 1 (output should be rotated or not)

This function was contributed by Kevin Mulholland.

=head1 AUTHOR

Kawai Takanori (Hippo2000) kwitknr@cpan.org

    http://member.nifty.ne.jp/hippo2000/            (Japanese)
    http://member.nifty.ne.jp/hippo2000/index_e.htm (English)

=head1 SEE ALSO

Spreadsheet::ParseExcel, Spreadsheet::WriteExcel

=head1 COPYRIGHT

This module is part of the Spreadsheet::ParseExcel distribution.

=cut
