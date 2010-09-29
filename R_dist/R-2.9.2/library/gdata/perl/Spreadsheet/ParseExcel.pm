# Spreadsheet::ParseExcel
#  by Kawai, Takanori (Hippo2000) 2000.10.2
#                                 2001. 2.2 (Ver. 0.15)
# This Program is ALPHA version.
#//////////////////////////////////////////////////////////////////////////////
# Spreadsheet::ParseExcel Objects
#//////////////////////////////////////////////////////////////////////////////
use Spreadsheet::ParseExcel::FmtDefault;
#==============================================================================
# Spreadsheet::ParseExcel::Workbook
#==============================================================================
package Spreadsheet::ParseExcel::Workbook;
use strict;
use warnings;

sub new {
    my ($class) = @_;
    my $self = {};
    bless $self, $class;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Workbook->ParseAbort
#------------------------------------------------------------------------------
sub ParseAbort {
    my($self, $val) =@_;
    $self->{_ParseAbort} = $val;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Workbook->Parse
#------------------------------------------------------------------------------
sub Parse {
    my($class, $source, $oFmt) =@_;
    my $excel = Spreadsheet::ParseExcel->new;
    my $workbook = $excel->Parse($source, $oFmt);
    $workbook->{_Excel} = $excel;
    return $workbook;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Workbook Worksheet
#------------------------------------------------------------------------------
sub Worksheet {
    my($oBook, $sName) =@_;
    my $oWkS;
    foreach $oWkS (@{$oBook->{Worksheet}}) {
        return $oWkS if($oWkS->{Name} eq $sName);
    }
    if($sName =~ /^\d+$/) {
        return $oBook->{Worksheet}->[$sName];
    }
    return undef;
}

#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}
#==============================================================================
# Spreadsheet::ParseExcel::Worksheet
#==============================================================================
package Spreadsheet::ParseExcel::Worksheet;
use strict;
use warnings;
use overload 
    '0+'        => \&sheetNo,
    'fallback'  => 1,
;
use Scalar::Util qw(weaken);

sub new {
    my ($class, %rhIni) = @_;
    my $self = \%rhIni;
    weaken $self->{_Book};

    $self->{Cells}=undef;
    $self->{DefColWidth}=8.38;
    bless $self, $class;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Worksheet->sheetNo
#------------------------------------------------------------------------------
sub sheetNo {
    my($oSelf) = @_;
    return $oSelf->{_SheetNo};
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Worksheet->Cell
#------------------------------------------------------------------------------
sub Cell {
    my($oSelf, $iR, $iC) = @_;

    # return undef if no arguments are given or if no cells are defined
    return  if ((!defined($iR)) || (!defined($iC)) ||
                (!defined($oSelf->{MaxRow})) || (!defined($oSelf->{MaxCol})));
    
    # return undef if outside defined rectangle
    return  if (($iR < $oSelf->{MinRow}) || ($iR > $oSelf->{MaxRow}) ||
                ($iC < $oSelf->{MinCol}) || ($iC > $oSelf->{MaxCol}));
    
    # return the Cell object
    return $oSelf->{Cells}[$iR][$iC];
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Worksheet->RowRange
#------------------------------------------------------------------------------
sub RowRange {
    my($oSelf) = @_;
    my $iMin = $oSelf->{MinRow} || 0;
    my $iMax = defined($oSelf->{MaxRow}) ? $oSelf->{MaxRow} : ($iMin-1);

    # return the range
    return($iMin, $iMax);
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel::Worksheet->ColRange
#------------------------------------------------------------------------------
sub ColRange {
    my($oSelf) = @_;
    my $iMin = $oSelf->{MinCol} || 0;
    my $iMax = defined($oSelf->{MaxCol}) ? $oSelf->{MaxCol} : ($iMin-1);

    # return the range
    return($iMin, $iMax);
}

#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}
#==============================================================================
# Spreadsheet::ParseExcel::Font
#==============================================================================
package Spreadsheet::ParseExcel::Font;
use strict;
use warnings;

sub new {
    my($class, %rhIni) = @_;
    my $self = \%rhIni;

    bless $self, $class;
}
#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}

#==============================================================================
# Spreadsheet::ParseExcel::Format
#==============================================================================
package Spreadsheet::ParseExcel::Format;
use strict;
use warnings;

sub new {
    my($class, %rhIni) = @_;
    my $self = \%rhIni;

    bless $self, $class;
}

#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}

#==============================================================================
# Spreadsheet::ParseExcel::Cell
#==============================================================================
package Spreadsheet::ParseExcel::Cell;
use strict;
use warnings;

sub new {
    my($sPkg, %rhKey)=@_;
    my($sWk, $iLen);
    my $self = \%rhKey;

    bless $self, $sPkg;
}

sub Value {
    my($self)=@_;
    return $self->{_Value};
}
#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}

#==============================================================================
# Spreadsheet::ParseExcel
#==============================================================================
package Spreadsheet::ParseExcel;
use strict;
use warnings;

use OLE::Storage_Lite;
use IO::File;
use Config;
our $VERSION = '0.32';

my @aColor =
(
    '000000',   # 0x00
    'FFFFFF', 'FFFFFF', 'FFFFFF', 'FFFFFF',
    'FFFFFF', 'FFFFFF', 'FFFFFF', 'FFFFFF', #0x08 - This one's Black, too ???
    'FFFFFF', 'FF0000', '00FF00', '0000FF',
    'FFFF00', 'FF00FF', '00FFFF', '800000', # 0x10
    '008000', '000080', '808000', '800080',
    '008080', 'C0C0C0', '808080', '9999FF', # 0x18
    '993366', 'FFFFCC', 'CCFFFF', '660066',
    'FF8080', '0066CC', 'CCCCFF', '000080', # 0x20
    'FF00FF', 'FFFF00', '00FFFF', '800080',
    '800000', '008080', '0000FF', '00CCFF', # 0x28
    'CCFFFF', 'CCFFCC', 'FFFF99', '99CCFF',
    'FF99CC', 'CC99FF', 'FFCC99', '3366FF', # 0x30
    '33CCCC', '99CC00', 'FFCC00', 'FF9900',
    'FF6600', '666699', '969696', '003366', # 0x38
    '339966', '003300', '333300', '993300',
    '993366', '333399', '333333', 'FFFFFF'  # 0x40
);
use constant verExcel95 => 0x500;
use constant verExcel97 =>0x600;
use constant verBIFF2 =>0x00;
use constant verBIFF3 =>0x02;
use constant verBIFF4 =>0x04;
use constant verBIFF5 =>0x08;
use constant verBIFF8 =>0x18;   #Added (Not in BOOK)

my %ProcTbl =(
#Develpers' Kit P291
    0x14    => \&_subHeader,            # Header
    0x15    => \&_subFooter,            # Footer
    0x18    => \&_subName,              # NAME(?)
    0x1A    => \&_subVPageBreak,        # Veritical Page Break
    0x1B    => \&_subHPageBreak,        # Horizontal Page Break
    0x22    => \&_subFlg1904,           # 1904 Flag
    0x26    => \&_subMergin,            # Left Mergin
    0x27    => \&_subMergin,            # Right Mergin
    0x28    => \&_subMergin,            # Top Mergin
    0x29    => \&_subMergin,            # Bottom Mergin
    0x2A    => \&_subPrintHeaders,      # Print Headers
    0x2B    => \&_subPrintGridlines,    # Print Gridlines
    0x3C    => \&_subContinue,          # Continue
    0x43    => \&_subXF,                # ExTended Format(?)
#Develpers' Kit P292
    0x55    => \&_subDefColWidth,       # Consider
    0x5C    => \&_subWriteAccess,       # WRITEACCESS
    0x7D    => \&_subColInfo,           # Colinfo
    0x7E    => \&_subRK,                # RK
    0x81    => \&_subWSBOOL,            # WSBOOL
    0x83    => \&_subHcenter,           # HCENTER
    0x84    => \&_subVcenter,           # VCENTER
    0x85    => \&_subBoundSheet,        # BoundSheet

    0x92    => \&_subPalette,           # Palette, fgp

    0x99    => \&_subStandardWidth,     # Standard Col
#Develpers' Kit P293
    0xA1    => \&_subSETUP,             # SETUP
    0xBD    => \&_subMulRK,             # MULRK
    0xBE    => \&_subMulBlank,          # MULBLANK
    0xD6    => \&_subRString,           # RString
#Develpers' Kit P294
    0xE0    => \&_subXF,                # ExTended Format
    0xE5    => \&_subMergeArea,         # MergeArea (Not Documented)
    0xFC    => \&_subSST,               # Shared String Table
    0xFD    => \&_subLabelSST,          # Label SST
#Develpers' Kit P295
    0x201   => \&_subBlank,             # Blank

    0x202   => \&_subInteger,           # Integer(Not Documented)
    0x203   => \&_subNumber,            # Number
    0x204   => \&_subLabel ,            # Label
    0x205   => \&_subBoolErr,           # BoolErr
    0x207   => \&_subString,            # STRING
    0x208   => \&_subRow,               # RowData
    0x221   => \&_subArray,             #Array (Consider)
    0x225   => \&_subDefaultRowHeight,  # Consider


    0x31    => \&_subFont,              # Font
    0x231   => \&_subFont,              # Font

    0x27E   => \&_subRK,                # RK
    0x41E   => \&_subFormat,            # Format

    0x06    => \&_subFormula,           # Formula
    0x406   => \&_subFormula,           # Formula

    0x09    => \&_subBOF,               # BOF(BIFF2)
    0x209   => \&_subBOF,               # BOF(BIFF3)
    0x409   => \&_subBOF,               # BOF(BIFF4)
    0x809   => \&_subBOF,               # BOF(BIFF5-8)
    );

my $BIGENDIAN;
my $PREFUNC;
my $_CellHandler;
my $_NotSetCell;
my $_Object;
my $_use_perlio;
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel->new
#------------------------------------------------------------------------------
sub new {
    my ($class, %hParam) =@_;

    if (not defined $_use_perlio) {
       if (exists $Config{useperlio} && $Config{useperlio} eq "define") {
           $_use_perlio = 1;
       } else {
           $_use_perlio = 0;
           require IO::Scalar;
           import  IO::Scalar;
       }
    }

    # Check ENDIAN(Little: Interl etc. BIG: Sparc etc)
    $BIGENDIAN = (defined $hParam{Endian})? $hParam{Endian} :
                    (unpack("H08", pack("L", 2)) eq '02000000')? 0: 1;
    my $self = {};
    bless $self, $class;

    $self->{GetContent} = \&_subGetContent;

    if ($hParam{EventHandlers}) {
        $self->SetEventHandlers($hParam{EventHandlers});
    } else {
        $self->SetEventHandlers(\%ProcTbl);
    }
    if($hParam{AddHandlers}) {
        foreach my $sKey (keys(%{$hParam{AddHandlers}})) {
            $self->SetEventHandler($sKey, $hParam{AddHandlers}->{$sKey});
        }
    }
    $_CellHandler = $hParam{CellHandler} if($hParam{CellHandler});
    $_NotSetCell  = $hParam{NotSetCell};
    $_Object      = $hParam{Object};

    return $self;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel->SetEventHandler
#------------------------------------------------------------------------------
sub SetEventHandler {
    my($self, $key, $sub_ref) = @_;
    $self->{FuncTbl}->{$key} = $sub_ref;
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel->SetEventHandlers
#------------------------------------------------------------------------------
sub SetEventHandlers {
    my($self, $rhTbl) = @_;
    $self->{FuncTbl} = undef;
    foreach my $sKey (keys %$rhTbl) {
        $self->{FuncTbl}->{$sKey} = $rhTbl->{$sKey};
    }
}
#------------------------------------------------------------------------------
# Spreadsheet::ParseExcel->Parse
#------------------------------------------------------------------------------
sub Parse {
    my($self, $source, $oWkFmt)=@_;

    my $oBook = Spreadsheet::ParseExcel::Workbook->new;
    $oBook->{SheetCount} = 0;

    my ($sBIFF, $iLen) = $self->_get_content($source, $oBook);
    return undef if not $sBIFF;

    if ($oWkFmt) {
        $oBook->{FmtClass} = $oWkFmt;
    } else {
        $oBook->{FmtClass} = Spreadsheet::ParseExcel::FmtDefault->new;
    }

    #3. Parse content
    my $lPos = 0;
    my $sWk = substr($sBIFF, $lPos, 4);
    $lPos += 4;
    my $iEfFlg = 0;
    while($lPos<=$iLen) {
        my($bOp, $bLen) = unpack("v2", $sWk);
        if($bLen) {
            $sWk = substr($sBIFF, $lPos, $bLen);
            $lPos += $bLen;
        }
        #printf STDERR "%4X:%s\n", $bOp, 'UNDEFIND---:' . unpack("H*", $sWk) unless($NameTbl{$bOp});
        #Check EF, EOF
        if($bOp == 0xEF) {    #EF
            $iEfFlg = $bOp;
        } elsif($bOp == 0x0A) { #EOF
            undef $iEfFlg;
        }
        #1. Formula String with No String 
        if (not $iEfFlg) {
            if($oBook->{_PrevPos} && (defined $self->{FuncTbl}->{$bOp}) && ($bOp != 0x207)) {
                my $iPos = $oBook->{_PrevPos};
                $oBook->{_PrevPos} = undef;
                my ($iR, $iC, $iF) = @$iPos; 
                _NewCell (
                    $oBook, $iR, $iC,
                    Kind    => 'Formula String',
                    Val     => '',
                    FormatNo=> $iF,
                    Format  => $oBook->{Format}[$iF],
                    Numeric => 0,
                    Code    => undef,
                    Book    => $oBook,
                );                         
            }
            if(defined $self->{FuncTbl}->{$bOp}) {
                $self->{FuncTbl}->{$bOp}->($oBook, $bOp, $bLen, $sWk);
            }
            $PREFUNC = $bOp if ($bOp != 0x3C); #Not Continue 
        }
        if (($lPos+4) <= $iLen) {
            $sWk = substr($sBIFF, $lPos, 4);
        }
        $lPos += 4;
        return $oBook if defined $oBook->{_ParseAbort};
    }
    return $oBook;
}

# $source is either filename or open filehandle or array of string or scalar
# referernce
# $oBook is passed to be updated
sub _get_content {
    my ($self, $source, $oBook) = @_;

    if(ref($source) eq "SCALAR") {
        #1.1 Specified by Buffer
        my ($sBIFF, $iLen) = $self->{GetContent}->($source);
        return $sBIFF ? ($sBIFF, $iLen) : (undef);
    }
        #1.2 Specified by Other Things(HASH reference etc)
        #    elsif(ref($source)) {
        #        return undef;
        #    }
        #1.2 Specified by GLOB reference
     elsif((ref($source) =~ /GLOB/) or
           (ref($source) eq 'Fh')) { #For CGI.pm (Light FileHandle)
        binmode($source);
        my $sWk;
        my $sBuff='';
        while(read($source, $sWk, 4096)) {
            $sBuff .= $sWk;
        }                
        my ($sBIFF, $iLen) = $self->{GetContent}->(\$sBuff);
        return $sBIFF ? ($sBIFF, $iLen) : (undef);
     } elsif(ref($source) eq 'ARRAY') {
        #1.3 Specified by File content
        $oBook->{File} = undef;
        my $sData = join('', @$source);
        my ($sBIFF, $iLen) = $self->{GetContent}->(\$sData);
        return $sBIFF ? ($sBIFF, $iLen) : (undef);
    } else {
        #1.4 Specified by File name
        $oBook->{File} = $source;
        return undef unless (-e $source);
        my ($sBIFF, $iLen) = $self->{GetContent}->($source);
        return $sBIFF ? ($sBIFF, $iLen) : (undef);
    }
}


#------------------------------------------------------------------------------
# _subGetContent (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _subGetContent {
    my ($sFile) = @_;
    
    my $oOl = OLE::Storage_Lite->new($sFile);
    return (undef, undef) unless($oOl);
    my @aRes = $oOl->getPpsSearch(
            [OLE::Storage_Lite::Asc2Ucs('Book'), 
             OLE::Storage_Lite::Asc2Ucs('Workbook')], 1, 1);
    return (undef, undef) if($#aRes < 0);
#Hack from Herbert
    if ($aRes[0]->{Data}) {
        return ($aRes[0]->{Data}, length($aRes[0]->{Data}));
    } 

    #Same as OLE::Storage_Lite
    my $oIo;
    #1. $sFile is Ref of scalar
    if(ref($sFile) eq 'SCALAR') {
        if ($_use_perlio) {
            open $oIo, "<", \$sFile;
        } else {
            $oIo = IO::Scalar->new;
            $oIo->open($sFile);
        }
    }
    #2. $sFile is a IO::Handle object
    elsif(UNIVERSAL::isa($sFile, 'IO::Handle')) {
        $oIo = $sFile;
        binmode($oIo);
    }
    #3. $sFile is a simple filename string
    elsif(!ref($sFile)) {
        $oIo = IO::File->new;
        $oIo->open("<$sFile") || return undef;
        binmode($oIo);
    }
    my $sWk;
    my $sBuff ='';

    while($oIo->read($sWk, 4096)) { #4_096 has no special meanings
        $sBuff .= $sWk;
    }
    $oIo->close();
    #Not Excel file (simple method)
    return (undef, undef) if (substr($sBuff, 0, 1) ne "\x09");
    return ($sBuff, length($sBuff));
}
#------------------------------------------------------------------------------
# _subBOF (for Spreadsheet::ParseExcel) Developers' Kit : P303
#------------------------------------------------------------------------------
sub _subBOF {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iVer, $iDt) = unpack("v2", $sWk);

    #Workbook Global
    if($iDt==0x0005) {
        $oBook->{Version} = unpack("v", $sWk);
        $oBook->{BIFFVersion} = 
                ($oBook->{Version}==verExcel95)? verBIFF5:verBIFF8;
        $oBook->{_CurSheet} = undef;
        $oBook->{_CurSheet_} = -1; 
    }
    #Worksheeet or Dialogsheet
    elsif($iDt != 0x0020) {  #if($iDt == 0x0010) 
        if(defined $oBook->{_CurSheet_}) {
            $oBook->{_CurSheet} = $oBook->{_CurSheet_} + 1;
            $oBook->{_CurSheet_}++; 

            ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{SheetVersion},
             $oBook->{Worksheet}[$oBook->{_CurSheet}]->{SheetType},) 
                    = unpack("v2", $sWk) if(length($sWk) > 4);
        }
        else {
            $oBook->{BIFFVersion} = int($bOp / 0x100);
            if (($oBook->{BIFFVersion} == verBIFF2) ||
                ($oBook->{BIFFVersion} == verBIFF3) ||
                ($oBook->{BIFFVersion} == verBIFF4)) {
                $oBook->{Version} = $oBook->{BIFFVersion};
                $oBook->{_CurSheet} = 0;
                $oBook->{Worksheet}[$oBook->{SheetCount}] =
                    Spreadsheet::ParseExcel::Worksheet->new(
                             _Name => '',
                              Name => '',
                             _Book => $oBook,
                            _SheetNo => $oBook->{SheetCount},
                        );
                $oBook->{SheetCount}++;
            }
        }
    }
    else {
        ($oBook->{_CurSheet_}, $oBook->{_CurSheet}) =
            (((defined $oBook->{_CurSheet})? $oBook->{_CurSheet}: -1), 
                undef);
    }
}
#------------------------------------------------------------------------------
# _subBlank (for Spreadsheet::ParseExcel) DK:P303
#------------------------------------------------------------------------------
sub _subBlank {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iR, $iC, $iF) = unpack("v3", $sWk);
    _NewCell(
            $oBook, $iR, $iC,
            Kind    => 'BLANK',
            Val     => '',
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => undef,
            Book    => $oBook,
        );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subInteger (for Spreadsheet::ParseExcel) Not in DK
#------------------------------------------------------------------------------
sub _subInteger {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iR, $iC, $iF, $sTxt, $sDum);

    ($iR, $iC, $iF, $sDum, $sTxt) = unpack("v3cv", $sWk);
    _NewCell (  
            $oBook, $iR, $iC,
                Kind    => 'INTEGER',
                Val     => $sTxt,
                FormatNo=> $iF,
                Format  => $oBook->{Format}[$iF],
                Numeric => 0,
                Code    => undef,
                Book    => $oBook,
            );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subNumber (for Spreadsheet::ParseExcel)  : DK: P354
#------------------------------------------------------------------------------
sub _subNumber {
    my($oBook, $bOp, $bLen, $sWk) = @_;

    my ($iR, $iC, $iF) = unpack("v3", $sWk);
    my $dVal = _convDval(substr($sWk, 6, 8));
    _NewCell (
                $oBook, $iR, $iC,
                Kind    => 'Number',
                Val     => $dVal,
                FormatNo=> $iF,
                Format  => $oBook->{Format}[$iF],
                Numeric => 1,
                Code    => undef,
                Book    => $oBook,
            );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _convDval (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _convDval {
    my($sWk)=@_;
    return  unpack("d", ($BIGENDIAN)? 
                    pack("c8", reverse(unpack("c8", $sWk))) : $sWk);
}
#------------------------------------------------------------------------------
# _subRString (for Spreadsheet::ParseExcel) DK:P405
#------------------------------------------------------------------------------
sub _subRString {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iR, $iC, $iF, $iL, $sTxt);
    ($iR, $iC, $iF, $iL) = unpack("v4", $sWk);
    $sTxt = substr($sWk, 8, $iL);

    #Has STRUN
    if(length($sWk) > (8+$iL)) {
        _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'RString',
            Val     => $sTxt,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => '_native_', #undef,
            Book    => $oBook,
            Rich    => substr($sWk, (8+$iL)+1),
        );
    }
    else {
        _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'RString',
            Val     => $sTxt,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => '_native_',
            Book    => $oBook,
        );
    }
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subBoolErr (for Spreadsheet::ParseExcel) DK:P306
#------------------------------------------------------------------------------
sub _subBoolErr {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iR, $iC, $iF) = unpack("v3", $sWk);
    my ($iVal, $iFlg) = unpack("cc", substr($sWk, 6, 2));
    my $sTxt = DecodeBoolErr($iVal, $iFlg);

    _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'BoolError',
            Val     => $sTxt,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => undef,
            Book    => $oBook,
        );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subRK (for Spreadsheet::ParseExcel)  DK:P401
#------------------------------------------------------------------------------
sub _subRK {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iR, $iC) = unpack("v3", $sWk);

    my($iF, $sTxt)= _UnpackRKRec(substr($sWk, 4, 6));
    _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'RK',
            Val     => $sTxt,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 1,
            Code    => undef,
            Book    => $oBook,
        );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subArray (for Spreadsheet::ParseExcel)   DK:P297
#------------------------------------------------------------------------------
sub _subArray {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iBR, $iER, $iBC, $iEC) = unpack("v2c2", $sWk);
    
}
#------------------------------------------------------------------------------
# _subFormula (for Spreadsheet::ParseExcel) DK:P336
#------------------------------------------------------------------------------
sub _subFormula {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iR, $iC, $iF) = unpack("v3", $sWk);

    my ($iFlg) = unpack("v", substr($sWk,12,2));
    if($iFlg == 0xFFFF) {
        my($iKind) = unpack("c", substr($sWk, 6, 1));
        my($iVal)  = unpack("c", substr($sWk, 8, 1));

        if(($iKind==1) or ($iKind==2)) {
            my $sTxt = ($iKind == 1)? DecodeBoolErr($iVal, 0):DecodeBoolErr($iVal, 1);
            _NewCell (
                    $oBook, $iR, $iC,
                    Kind    => 'Formulra Bool',
                    Val     => $sTxt,
                    FormatNo=> $iF,
                    Format  => $oBook->{Format}[$iF],
                    Numeric => 0,
                    Code    => undef,
                    Book    => $oBook,
                );
        }
        else { # Result (Reserve Only)
            $oBook->{_PrevPos} = [$iR, $iC, $iF];
        }
    }
    else {
        my $dVal = _convDval(substr($sWk, 6, 8));
        _NewCell (
                $oBook, $iR, $iC,
                Kind    => 'Formula Number',
                Val     => $dVal,
                FormatNo=> $iF,
                Format  => $oBook->{Format}[$iF],
                Numeric => 1,
                Code    => undef,
                Book    => $oBook,
            );
    }
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subString (for Spreadsheet::ParseExcel)  DK:P414
#------------------------------------------------------------------------------
sub _subString {
    my($oBook, $bOp, $bLen, $sWk) = @_;
#Position (not enough for ARRAY)

    my $iPos = $oBook->{_PrevPos};
    return undef unless($iPos);
    $oBook->{_PrevPos} = undef;
    my ($iR, $iC, $iF) = @$iPos;

    my ($iLen, $sTxt, $sCode);
    if($oBook->{BIFFVersion} == verBIFF8) {
        my( $raBuff, $iLen) = _convBIFF8String($oBook, $sWk, 1);
        $sTxt  = $raBuff->[0];
        $sCode = ($raBuff->[1])? 'ucs2': undef;
    }
    elsif($oBook->{BIFFVersion} == verBIFF5) {
        $sCode = '_native_';
        $iLen = unpack("v", $sWk);
        $sTxt = substr($sWk, 2, $iLen);
    }
    else {
        $sCode = '_native_';
        $iLen = unpack("c", $sWk);
        $sTxt = substr($sWk, 1, $iLen);
    }
    _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'String',
            Val     => $sTxt,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => $sCode,
            Book    => $oBook,
        );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subLabel (for Spreadsheet::ParseExcel)   DK:P344
#------------------------------------------------------------------------------
sub _subLabel {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iR, $iC, $iF) = unpack("v3", $sWk);
    my ($sLbl, $sCode);
    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
        my ( $raBuff, $iLen, $iStPos, $iLenS) = 
                _convBIFF8String($oBook, substr($sWk,6), 1);
        $sLbl  = $raBuff->[0];
        $sCode = ($raBuff->[1])? 'ucs2': undef;
    }
    #Before BIFF8
    else {
        $sLbl  = substr($sWk,8);
        $sCode = '_native_';
    }
    _NewCell ( 
            $oBook, $iR, $iC,
            Kind    => 'Label',
            Val     => $sLbl,
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => $sCode,
            Book    => $oBook,
        );
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subMulRK (for Spreadsheet::ParseExcel)   DK:P349
#------------------------------------------------------------------------------
sub _subMulRK {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return if ($oBook->{SheetCount}<=0);

    my ($iR, $iSc) = unpack("v2", $sWk);
    my $iEc = unpack("v", substr($sWk, length($sWk) -2, 2));

    my $iPos = 4;
    for(my $iC=$iSc; $iC<=$iEc; $iC++) {
        my($iF, $lVal) = _UnpackRKRec(substr($sWk, $iPos, 6), $iR, $iC);
        _NewCell (
                $oBook, $iR, $iC,
                Kind    => 'MulRK',
                Val     => $lVal,
                FormatNo=> $iF,
                Format  => $oBook->{Format}[$iF],
                Numeric => 1,
                Code => undef,
                Book    => $oBook,
                );
        $iPos += 6;
    }
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iSc, $iEc);
}
#------------------------------------------------------------------------------
# _subMulBlank (for Spreadsheet::ParseExcel) DK:P349
#------------------------------------------------------------------------------
sub _subMulBlank {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iR, $iSc) = unpack("v2", $sWk);
    my $iEc = unpack("v", substr($sWk, length($sWk)-2, 2));
    my $iPos = 4;
    for(my $iC=$iSc; $iC<=$iEc; $iC++) {
        my $iF = unpack('v', substr($sWk, $iPos, 2));
        _NewCell (
                $oBook, $iR, $iC,
                Kind    => 'MulBlank',
                Val     => '',
                FormatNo=> $iF,
                Format  => $oBook->{Format}[$iF],
                Numeric => 0,
                Code    => undef,
                Book    => $oBook,
            );
        $iPos+=2;
    }
#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iSc, $iEc);
}
#------------------------------------------------------------------------------
# _subLabelSST (for Spreadsheet::ParseExcel) DK: P345
#------------------------------------------------------------------------------
sub _subLabelSST {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my ($iR, $iC, $iF, $iIdx) = unpack('v3V', $sWk);

    _NewCell (
            $oBook, $iR, $iC,
            Kind    => 'PackedIdx',
            Val     => $oBook->{PkgStr}[$iIdx]->{Text},
            FormatNo=> $iF,
            Format  => $oBook->{Format}[$iF],
            Numeric => 0,
            Code    => ($oBook->{PkgStr}[$iIdx]->{Unicode})? 'ucs2': undef,
            Book    => $oBook,
            Rich   => $oBook->{PkgStr}[$iIdx]->{Rich},
        );

#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iC, $iC);
}
#------------------------------------------------------------------------------
# _subFlg1904 (for Spreadsheet::ParseExcel) DK:P296
#------------------------------------------------------------------------------
sub _subFlg1904 {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    $oBook->{Flg1904} = unpack("v", $sWk);
}
#------------------------------------------------------------------------------
# _subRow (for Spreadsheet::ParseExcel) DK:P403
#------------------------------------------------------------------------------
sub _subRow {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

#0. Get Worksheet info (MaxRow, MaxCol, MinRow, MinCol)
    my($iR, $iSc, $iEc, $iHght, $undef1, $undef2, $iGr, $iXf) = unpack("v8", $sWk);
    $iEc--;

#1. RowHeight
    if($iGr & 0x20) {   #Height = 0
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{RowHeight}[$iR] = 0;
    }
    else {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{RowHeight}[$iR] = $iHght/20.0;
    }

#2.MaxRow, MaxCol, MinRow, MinCol
    _SetDimension($oBook, $iR, $iSc, $iEc);
}
#------------------------------------------------------------------------------
# _SetDimension (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _SetDimension {
    my($oBook, $iR, $iSc, $iEc)=@_;
    return undef unless(defined $oBook->{_CurSheet});

#2.MaxRow, MaxCol, MinRow, MinCol
#2.1 MinRow
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinRow} = $iR 
        unless (defined $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinRow}) and 
               ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinRow} <= $iR);

#2.2 MaxRow
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxRow} = $iR 
        unless (defined $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxRow}) and
               ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxRow} > $iR);
#2.3 MinCol
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinCol} = $iSc
            unless (defined $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinCol}) and
               ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{MinCol} <= $iSc);
#2.4 MaxCol
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxCol} = $iEc 
            unless (defined $oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxCol}) and
               ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{MaxCol} > $iEc);

}
#------------------------------------------------------------------------------
# _subDefaultRowHeight (for Spreadsheet::ParseExcel)    DK: P318
#------------------------------------------------------------------------------
sub _subDefaultRowHeight {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});
#1. RowHeight
    my($iDum, $iHght) = unpack("v2", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{DefRowHeight} = $iHght/20;

}
#------------------------------------------------------------------------------
# _subStandardWidth(for Spreadsheet::ParseExcel)    DK:P413
#------------------------------------------------------------------------------
sub _subStandardWidth {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my $iW = unpack("v", $sWk);
    $oBook->{StandardWidth}= _adjustColWidth($oBook, $iW);
}
#------------------------------------------------------------------------------
# _subDefColWidth(for Spreadsheet::ParseExcel)      DK:P319
#------------------------------------------------------------------------------
sub _subDefColWidth {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});
    my $iW = unpack("v", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{DefColWidth}= _adjustColWidth($oBook, $iW);
}
#------------------------------------------------------------------------------
# _adjustColWidth (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _adjustColWidth {
    my($oBook, $iW)=@_;
    return (($iW -0xA0)/256);
#    ($oBook->{Worksheet}[$oBook->{_CurSheet}]->{SheetVersion} == verExcel97)?
#        (($iW -0xA0)/256) : $iW;
}
#------------------------------------------------------------------------------
# _subColInfo (for Spreadsheet::ParseExcel) DK:P309
#------------------------------------------------------------------------------
sub _subColInfo {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});
    my($iSc, $iEc, $iW, $iXF, $iGr) = unpack("v5", $sWk);
    for(my $i= $iSc; $i<=$iEc; $i++) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{ColWidth}[$i] = 
                        ($iGr & 0x01)? 0: _adjustColWidth($oBook, $iW);
                    #0x01 means HIDDEN
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{ColFmtNo}[$i] = $iXF;
        # $oBook->{Worksheet}[$oBook->{_CurSheet}]->{ColCr}[$i]    = $iGr; #Not Implemented
    }
}
#------------------------------------------------------------------------------
# _subSST (for Spreadsheet::ParseExcel) DK:P413
#------------------------------------------------------------------------------
sub _subSST {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    _subStrWk($oBook, substr($sWk, 8));
}
#------------------------------------------------------------------------------
# _subContinue (for Spreadsheet::ParseExcel)    DK:P311
#------------------------------------------------------------------------------
sub _subContinue {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    #if(defined $self->{FuncTbl}->{$bOp}) {
    #    $self->{FuncTbl}->{$PREFUNC}->($oBook, $bOp, $bLen, $sWk);
    #}

    _subStrWk($oBook, $sWk, 1) if($PREFUNC == 0xFC);
}
#------------------------------------------------------------------------------
# _subWriteAccess (for Spreadsheet::ParseExcel) DK:P451
#------------------------------------------------------------------------------
sub _subWriteAccess {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return if (defined $oBook->{_Author});

    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
        $oBook->{Author} = _convBIFF8String($oBook, $sWk);
    }
    #Before BIFF8
    else {
        my($iLen) = unpack("c", $sWk);
        $oBook->{Author} = $oBook->{FmtClass}->TextFmt(substr($sWk, 1, $iLen), '_native_');
    }
}
#------------------------------------------------------------------------------
# _convBIFF8String (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _convBIFF8String {
    my($oBook, $sWk, $iCnvFlg) = @_;
    my($iLen, $iFlg) = unpack("vc", $sWk);
    my($iHigh, $iExt, $iRich) = ($iFlg & 0x01, $iFlg & 0x04, $iFlg & 0x08);
    my($iStPos, $iExtCnt, $iRichCnt, $sStr);
#2. Rich and Ext
    if($iRich && $iExt) {
        $iStPos   = 9;
        ($iRichCnt, $iExtCnt) = unpack('vV', substr($sWk, 3, 6));
    }
    elsif($iRich) { #Only Rich
        $iStPos   = 5;
        $iRichCnt = unpack('v', substr($sWk, 3, 2));
        $iExtCnt  = 0;
    }
    elsif($iExt)  { #Only Ext
        $iStPos   = 7;
        $iRichCnt = 0;
        $iExtCnt  = unpack('V', substr($sWk, 3, 4));
    }
    else {          #Nothing Special
        $iStPos   = 3;
        $iExtCnt  = 0;
        $iRichCnt = 0;
    }
#3.Get String
    if($iHigh) {    #Compressed
        $iLen *= 2;
        $sStr = substr($sWk,    $iStPos, $iLen);
        _SwapForUnicode(\$sStr);
        $sStr = $oBook->{FmtClass}->TextFmt($sStr, 'ucs2') unless($iCnvFlg);
    }
    else {              #Not Compressed
        $sStr = substr($sWk, $iStPos, $iLen);
        $sStr = $oBook->{FmtClass}->TextFmt($sStr, undef) unless($iCnvFlg);
    }

#4. return 
    if(wantarray) {
        #4.1 Get Rich and Ext
        if(length($sWk) < $iStPos + $iLen+ $iRichCnt*4+$iExtCnt) {
            return ([undef, $iHigh, undef, undef], 
                $iStPos + $iLen+ $iRichCnt*4+$iExtCnt, $iStPos, $iLen);
        }
        else {
            return ([$sStr, $iHigh,
                    substr($sWk, $iStPos + $iLen, $iRichCnt*4),
                    substr($sWk, $iStPos + $iLen+ $iRichCnt*4, $iExtCnt)], 
                $iStPos + $iLen+ $iRichCnt*4+$iExtCnt,  $iStPos, $iLen);
        }
    }
    else {
        return $sStr;
    }
}
#------------------------------------------------------------------------------
# _subXF (for Spreadsheet::ParseExcel)     DK:P453
#------------------------------------------------------------------------------
sub _subXF {
    my($oBook, $bOp, $bLen, $sWk) = @_;

    my ($iFnt, $iIdx);
    my($iLock, $iHidden, $iStyle, $i123, $iAlH, $iWrap, $iAlV, $iJustL, $iRotate,
        $iInd, $iShrink, $iMerge, $iReadDir, $iBdrD,
        $iBdrSL, $iBdrSR, $iBdrST, $iBdrSB, $iBdrSD,
        $iBdrCL, $iBdrCR, $iBdrCT, $iBdrCB, $iBdrCD,
        $iFillP, $iFillCF, $iFillCB);

    if($oBook->{BIFFVersion} == verBIFF8) {
        my ($iGen, $iAlign, $iGen2, $iBdr1,  $iBdr2, $iBdr3, $iPtn );

        ($iFnt, $iIdx, $iGen, $iAlign, $iGen2, $iBdr1,  $iBdr2, $iBdr3, $iPtn )
            = unpack("v7Vv", $sWk);
        $iLock   = ($iGen & 0x01)? 1:0;
        $iHidden = ($iGen & 0x02)? 1:0;
        $iStyle  = ($iGen & 0x04)? 1:0;
        $i123    = ($iGen & 0x08)? 1:0;
        $iAlH    = ($iAlign & 0x07);
        $iWrap   = ($iAlign & 0x08)? 1:0;
        $iAlV    = ($iAlign & 0x70) / 0x10;
        $iJustL  = ($iAlign & 0x80)? 1:0;

        $iRotate  = (($iAlign & 0xFF00) / 0x100) & 0x00FF;
        $iRotate = 90 if($iRotate == 255);
        $iRotate = 90 - $iRotate if($iRotate > 90);

        $iInd     = ($iGen2 & 0x0F);
        $iShrink  = ($iGen2 & 0x10)? 1:0;
        $iMerge   = ($iGen2 & 0x20)? 1:0;
        $iReadDir = (($iGen2 & 0xC0) / 0x40) & 0x03;
        $iBdrSL = $iBdr1 & 0x0F;
        $iBdrSR = (($iBdr1 & 0xF0)   / 0x10)   & 0x0F;
        $iBdrST = (($iBdr1 & 0xF00)  / 0x100)  & 0x0F;
        $iBdrSB = (($iBdr1 & 0xF000) / 0x1000) & 0x0F;

        $iBdrCL = (($iBdr2 & 0x7F)) & 0x7F;
        $iBdrCR = (($iBdr2 & 0x3F80) / 0x80) & 0x7F;
        $iBdrD  = (($iBdr2 & 0xC000) / 0x4000) & 0x3;

        $iBdrCT = (($iBdr3 & 0x7F)) & 0x7F;
        $iBdrCB = (($iBdr3 & 0x3F80) / 0x80) & 0x7F;
        $iBdrCD = (($iBdr3 & 0x1FC000) / 0x4000) & 0x7F;
        $iBdrSD = (($iBdr3 & 0x1E00000) / 0x200000) & 0xF;
        $iFillP = (($iBdr3 & 0xFC000000) / 0x4000000) & 0x3F;

        $iFillCF = ($iPtn & 0x7F);
        $iFillCB = (($iPtn & 0x3F80) / 0x80) & 0x7F;
    }
    else {
        my ($iGen, $iAlign, $iPtn,  $iPtn2, $iBdr1, $iBdr2);

        ($iFnt, $iIdx, $iGen, $iAlign, $iPtn,  $iPtn2, $iBdr1, $iBdr2)
            = unpack("v8", $sWk);
        $iLock   = ($iGen & 0x01)? 1:0;
        $iHidden = ($iGen & 0x02)? 1:0;
        $iStyle  = ($iGen & 0x04)? 1:0;
        $i123    = ($iGen & 0x08)? 1:0;

        $iAlH    = ($iAlign & 0x07);
        $iWrap  = ($iAlign & 0x08)? 1:0;
        $iAlV    = ($iAlign & 0x70) / 0x10;
        $iJustL  = ($iAlign & 0x80)? 1:0;

        $iRotate  = (($iAlign & 0x300) / 0x100) & 0x3;

        $iFillCF = ($iPtn & 0x7F);
        $iFillCB = (($iPtn & 0x1F80) / 0x80) & 0x7F;

        $iFillP = ($iPtn2 & 0x3F);
        $iBdrSB  = (($iPtn2 & 0x1C0) /  0x40) & 0x7;
        $iBdrCB = (($iPtn2 & 0xFE00) / 0x200) & 0x7F;

        $iBdrST = ($iBdr1 & 0x07);
        $iBdrSL = (($iBdr1 & 0x38)   / 0x8)   & 0x07;
        $iBdrSR = (($iBdr1 & 0x1C0)  / 0x40)  & 0x07;
        $iBdrCT = (($iBdr1 & 0xFE00) / 0x200)  & 0x7F;

        $iBdrCL = ($iBdr2  & 0x7F)  & 0x7F;
        $iBdrCR = (($iBdr2 & 0x3F80) / 0x80) & 0x7F;
    }

   push @{$oBook->{Format}} , 
         Spreadsheet::ParseExcel::Format->new (
            FontNo   => $iFnt,
            Font     => $oBook->{Font}[$iFnt], 
            FmtIdx   => $iIdx,

            Lock     => $iLock,
            Hidden   => $iHidden,
            Style    => $iStyle,
            Key123   => $i123,
            AlignH   => $iAlH,
            Wrap     => $iWrap,
            AlignV   => $iAlV,
            JustLast => $iJustL,
            Rotate   => $iRotate,

            Indent   => $iInd,
            Shrink   => $iShrink,
            Merge    => $iMerge,
            ReadDir  => $iReadDir,

            BdrStyle => [$iBdrSL, $iBdrSR, $iBdrST, $iBdrSB],
            BdrColor => [$iBdrCL, $iBdrCR, $iBdrCT, $iBdrCB],
            BdrDiag  => [$iBdrD, $iBdrSD, $iBdrCD],
            Fill     => [$iFillP, $iFillCF, $iFillCB],
        );
}
#------------------------------------------------------------------------------
# _subFormat (for Spreadsheet::ParseExcel)  DK: P336
#------------------------------------------------------------------------------
sub _subFormat {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my $sFmt;
    if (($oBook->{BIFFVersion} == verBIFF2) ||
        ($oBook->{BIFFVersion} == verBIFF3) ||
        ($oBook->{BIFFVersion} == verBIFF4) ||
        ($oBook->{BIFFVersion} == verBIFF5) ) {
        $sFmt = substr($sWk, 3, unpack('c', substr($sWk, 2, 1)));
        $sFmt = $oBook->{FmtClass}->TextFmt($sFmt, '_native_');
    }
    else {
        $sFmt = _convBIFF8String($oBook, substr($sWk, 2));
    }
    $oBook->{FormatStr}->{unpack('v', substr($sWk, 0, 2))} = $sFmt;
}
#------------------------------------------------------------------------------
# _subPalette (for Spreadsheet::ParseExcel) DK: P393
#------------------------------------------------------------------------------
sub _subPalette {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    for(my $i=0;$i<unpack('v', $sWk);$i++) {
#        push @aColor, unpack('H6', substr($sWk, $i*4+2));
        $aColor[$i+8] = unpack('H6', substr($sWk, $i*4+2));
    }
}
#------------------------------------------------------------------------------
# _subFont (for Spreadsheet::ParseExcel) DK:P333
#------------------------------------------------------------------------------
sub _subFont {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iHeight, $iAttr, $iCIdx, $iBold, $iSuper, $iUnderline, $sFntName);
    my($bBold, $bItalic, $bUnderline, $bStrikeout);

    if($oBook->{BIFFVersion} == verBIFF8) {
        ($iHeight, $iAttr, $iCIdx, $iBold, $iSuper, $iUnderline) = 
            unpack("v5c", $sWk);
        my($iSize, $iHigh) = unpack('cc', substr($sWk, 14, 2));
        if($iHigh) {
            $sFntName = substr($sWk, 16, $iSize*2);
            _SwapForUnicode(\$sFntName);
            $sFntName = $oBook->{FmtClass}->TextFmt($sFntName, 'ucs2');
        }
        else {
            $sFntName = substr($sWk, 16, $iSize);
            $sFntName = $oBook->{FmtClass}->TextFmt($sFntName, '_native_');
        }
        $bBold       = ($iBold >= 0x2BC)? 1: 0;
        $bItalic     = ($iAttr & 0x02)? 1: 0;
        $bStrikeout  = ($iAttr & 0x08)? 1: 0;
        $bUnderline  = ($iUnderline)? 1: 0;
    }
    elsif($oBook->{BIFFVersion} == verBIFF5) {
        ($iHeight, $iAttr, $iCIdx, $iBold, $iSuper, $iUnderline) = 
            unpack("v5c", $sWk);
        $sFntName = $oBook->{FmtClass}->TextFmt(
                    substr($sWk, 15, unpack("c", substr($sWk, 14, 1))), 
                    '_native_');
        $bBold       = ($iBold >= 0x2BC)? 1: 0;
        $bItalic     = ($iAttr & 0x02)? 1: 0;
        $bStrikeout  = ($iAttr & 0x08)? 1: 0;
        $bUnderline  = ($iUnderline)? 1: 0;
    }
    else {
        ($iHeight, $iAttr) = unpack("v2", $sWk);
        $iCIdx       = undef;
        $iSuper      = 0;

        $bBold       = ($iAttr & 0x01)? 1: 0;
        $bItalic     = ($iAttr & 0x02)? 1: 0;
        $bUnderline  = ($iAttr & 0x04)? 1: 0;
        $bStrikeout  = ($iAttr & 0x08)? 1: 0;

        $sFntName = substr($sWk, 5, unpack("c", substr($sWk, 4, 1)));
    }
    push @{$oBook->{Font}}, 
        Spreadsheet::ParseExcel::Font->new(
            Height          => $iHeight / 20.0,
            Attr            => $iAttr,
            Color           => $iCIdx,
            Super           => $iSuper,
            UnderlineStyle  => $iUnderline,
            Name            => $sFntName,

            Bold            => $bBold,
            Italic          => $bItalic,
            Underline       => $bUnderline,
            Strikeout       => $bStrikeout,
    );
    #Skip Font[4]
    push @{$oBook->{Font}}, {} if(scalar(@{$oBook->{Font}}) == 4);

}
#------------------------------------------------------------------------------
# _subBoundSheet (for Spreadsheet::ParseExcel): DK: P307
#------------------------------------------------------------------------------
sub _subBoundSheet {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iPos, $iGr, $iKind) = unpack("Lc2", $sWk);
    $iKind &= 0x0F;
    return if(($iKind != 0x00) && ($iKind != 0x01));

    if($oBook->{BIFFVersion} >= verBIFF8) {
        my($iSize, $iUni) = unpack("cc", substr($sWk, 6, 2));
        my $sWsName = substr($sWk, 8);
        if($iUni & 0x01) {
            _SwapForUnicode(\$sWsName);
            $sWsName = $oBook->{FmtClass}->TextFmt($sWsName, 'ucs2');
        }
        $oBook->{Worksheet}[$oBook->{SheetCount}] = 
            Spreadsheet::ParseExcel::Worksheet->new(
                    Name => $sWsName,
                    Kind => $iKind,
                    _Pos => $iPos,
                    _Book => $oBook,
                    _SheetNo => $oBook->{SheetCount},
                );
    }
    else {
        $oBook->{Worksheet}[$oBook->{SheetCount}] = 
            Spreadsheet::ParseExcel::Worksheet->new(
                    Name => $oBook->{FmtClass}->TextFmt(substr($sWk, 7), '_native_'),
                    Kind => $iKind,
                    _Pos => $iPos,
                    _Book => $oBook,
                    _SheetNo => $oBook->{SheetCount},
                );
    }
    $oBook->{SheetCount}++;
}
#------------------------------------------------------------------------------
# _subHeader (for Spreadsheet::ParseExcel) DK: P340
#------------------------------------------------------------------------------
sub _subHeader {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});
    my $sW;
    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
    $sW = _convBIFF8String($oBook, $sWk);
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{Header} = 
        ($sW eq "\x00")? undef : $sW;
    }
    #Before BIFF8
    else {
        my($iLen) = unpack("c", $sWk);
    $sW = $oBook->{FmtClass}->TextFmt(substr($sWk, 1, $iLen), '_native_');
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{Header} =
        ($sW eq "\x00\x00\x00")? undef : $sW;
    }
}
#------------------------------------------------------------------------------
# _subFooter (for Spreadsheet::ParseExcel) DK: P335
#------------------------------------------------------------------------------
sub _subFooter {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});
    my $sW;
    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
    $sW = _convBIFF8String($oBook, $sWk);
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{Footer} = 
        ($sW eq "\x00")? undef : $sW;
    }
    #Before BIFF8
    else {
        my($iLen) = unpack("c", $sWk);
    $sW = $oBook->{FmtClass}->TextFmt(substr($sWk, 1, $iLen), '_native_');
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{Footer} = 
        ($sW eq "\x00\x00\x00")? undef : $sW;
    }
}
#------------------------------------------------------------------------------
# _subHPageBreak (for Spreadsheet::ParseExcel) DK: P341
#------------------------------------------------------------------------------
sub _subHPageBreak {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my @aBreak;
    my $iCnt = unpack("v", $sWk);

    return undef unless(defined $oBook->{_CurSheet});
    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
        for(my $i=0;$i<$iCnt;$i++) {
            my($iRow, $iColB, $iColE) = 
                    unpack('v3', substr($sWk, 2 + $i*6, 6));
#            push @aBreak, [$iRow, $iColB, $iColE];
            push @aBreak, $iRow;
        }
    }
    #Before BIFF8
    else {
        for(my $i=0;$i<$iCnt;$i++) {
            my($iRow) = 
                    unpack('v', substr($sWk, 2 + $i*2, 2));
            push @aBreak, $iRow;
#            push @aBreak, [$iRow, 0, 255];
        }
    }
    @aBreak = sort {$a <=> $b} @aBreak;
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{HPageBreak} = \@aBreak;
}
#------------------------------------------------------------------------------
# _subVPageBreak (for Spreadsheet::ParseExcel) DK: P447
#------------------------------------------------------------------------------
sub _subVPageBreak {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my @aBreak;
    my $iCnt = unpack("v", $sWk);
    #BIFF8
    if($oBook->{BIFFVersion} >= verBIFF8) {
        for(my $i=0;$i<$iCnt;$i++) {
            my($iCol, $iRowB, $iRowE) = 
                    unpack('v3', substr($sWk, 2 + $i*6, 6));
            push @aBreak, $iCol;
#            push @aBreak, [$iCol, $iRowB, $iRowE];
        }
    }
    #Before BIFF8
    else {
        for(my $i=0;$i<$iCnt;$i++) {
            my($iCol) = 
                    unpack('v', substr($sWk, 2 + $i*2, 2));
            push @aBreak, $iCol;
#            push @aBreak, [$iCol, 0, 65535];
        }
    }
    @aBreak = sort {$a <=> $b} @aBreak;
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{VPageBreak} = \@aBreak;
}
#------------------------------------------------------------------------------
# _subMergin (for Spreadsheet::ParseExcel) DK: P306, 345, 400, 440
#------------------------------------------------------------------------------
sub _subMergin {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $dWk = _convDval(substr($sWk, 0, 8)) * 127 / 50;
    if($bOp == 0x26) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{LeftMergin} = $dWk;
    }
    elsif($bOp == 0x27) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{RightMergin} = $dWk;
    }
    elsif($bOp == 0x28) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{TopMergin} = $dWk;
    }
    elsif($bOp == 0x29) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{BottomMergin} = $dWk;
    }
}
#------------------------------------------------------------------------------
# _subHcenter (for Spreadsheet::ParseExcel) DK: P340
#------------------------------------------------------------------------------
sub _subHcenter {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $iWk = unpack("v", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{HCenter} = $iWk;

}
#------------------------------------------------------------------------------
# _subVcenter (for Spreadsheet::ParseExcel) DK: P447
#------------------------------------------------------------------------------
sub _subVcenter {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $iWk = unpack("v", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{VCenter} = $iWk;
}
#------------------------------------------------------------------------------
# _subPrintGridlines (for Spreadsheet::ParseExcel) DK: P397
#------------------------------------------------------------------------------
sub _subPrintGridlines {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $iWk = unpack("v", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{PrintGrid} = $iWk;

}
#------------------------------------------------------------------------------
# _subPrintHeaders (for Spreadsheet::ParseExcel) DK: P397
#------------------------------------------------------------------------------
sub _subPrintHeaders {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $iWk = unpack("v", $sWk);
    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{PrintHeaders} = $iWk;
}
#------------------------------------------------------------------------------
# _subSETUP (for Spreadsheet::ParseExcel) DK: P409
#------------------------------------------------------------------------------
sub _subSETUP {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $oWkS = $oBook->{Worksheet}[$oBook->{_CurSheet}];
    my $iGrBit;

    ($oWkS->{PaperSize},
     $oWkS->{Scale}    ,
     $oWkS->{PageStart},
     $oWkS->{FitWidth} ,
     $oWkS->{FitHeight},
     $iGrBit,
     $oWkS->{Res},
     $oWkS->{VRes},) = unpack('v8', $sWk);

    $oWkS->{HeaderMergin} = _convDval(substr($sWk, 16, 8)) * 127 / 50;
    $oWkS->{FooterMergin} = _convDval(substr($sWk, 24, 8)) * 127 / 50;
    $oWkS->{Copis}= unpack('v2', substr($sWk, 32, 2));
    $oWkS->{LeftToRight}= (($iGrBit & 0x01)? 1: 0);
    $oWkS->{Landscape}  = (($iGrBit & 0x02)? 1: 0);
    $oWkS->{NoPls}      = (($iGrBit & 0x04)? 1: 0);
    $oWkS->{NoColor}    = (($iGrBit & 0x08)? 1: 0);
    $oWkS->{Draft}      = (($iGrBit & 0x10)? 1: 0);
    $oWkS->{Notes}      = (($iGrBit & 0x20)? 1: 0);
    $oWkS->{NoOrient}   = (($iGrBit & 0x40)? 1: 0);
    $oWkS->{UsePage}    = (($iGrBit & 0x80)? 1: 0);
}
#------------------------------------------------------------------------------
# _subName (for Spreadsheet::ParseExcel) DK: P350
#------------------------------------------------------------------------------
sub _subName {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    my($iGrBit, 
        $cKey, $cCh, 
        $iCce, $ixAls, $iTab,
        $cchCust, $cchDsc, $cchHep, $cchStatus) = unpack('vc2v3c4', $sWk);
#Builtin Name + Length == 1
    if(($iGrBit & 0x20) && ($cCh == 1)) {
        #BIFF8
        if($oBook->{BIFFVersion} >= verBIFF8) {
            my $iName  = unpack('n', substr($sWk, 14 ));
            my $iSheet = unpack('v', substr($sWk, 8 )) - 1;
            if($iName == 6) {       #PrintArea
                my($iSheetW, $raArea) = _ParseNameArea(substr($sWk, 16));
                $oBook->{PrintArea}[$iSheet] =  $raArea;
            }
            elsif($iName == 7) {    #Title
                my($iSheetW, $raArea) = _ParseNameArea(substr($sWk, 16));
                my @aTtlR = ();
                my @aTtlC = ();
                foreach my $raI (@$raArea) {
                    if($raI->[3] == 0xFF) { #Row Title
                        push @aTtlR, [$raI->[0], $raI->[2] ];
                    }
                    else {                  #Col Title
                        push @aTtlC, [$raI->[1], $raI->[3] ];
                    }
                }
                $oBook->{PrintTitle}[$iSheet] =  {Row => \@aTtlR, Column => \@aTtlC};
            }
        }
        else {
            my $iName = unpack('c', substr($sWk, 14 ));
            if($iName == 6) {       #PrintArea
                my($iSheet, $raArea) = _ParseNameArea95(substr($sWk, 15));
                $oBook->{PrintArea}[$iSheet] =  $raArea;
            }
            elsif($iName == 7) {    #Title
                my($iSheet, $raArea) = _ParseNameArea95(substr($sWk, 15));
                my @aTtlR = ();
                my @aTtlC = ();
                foreach my $raI (@$raArea) {
                    if($raI->[3] == 0xFF) { #Row Title
                        push @aTtlR, [$raI->[0], $raI->[2] ];
                    }
                    else {                  #Col Title
                        push @aTtlC, [$raI->[1], $raI->[3] ];
                    }
                }
                $oBook->{PrintTitle}[$iSheet] =  {Row => \@aTtlR, Column => \@aTtlC};
            }
        }
    }
}
#------------------------------------------------------------------------------
# ParseNameArea (for Spreadsheet::ParseExcel) DK: 494 (ptgAread3d)
#------------------------------------------------------------------------------
sub _ParseNameArea {
    my ($sObj) =@_;
    my ($iOp);
    my @aRes = ();
    $iOp = unpack('C', $sObj);
    my $iSheet;
    if($iOp == 0x3b) {
        my($iWkS, $iRs, $iRe, $iCs, $iCe) = 
            unpack('v5', substr($sObj, 1));
        $iSheet = $iWkS;
        push @aRes, [$iRs, $iCs, $iRe, $iCe];
    }
    elsif($iOp == 0x29) {
        my $iLen = unpack('v', substr($sObj, 1, 2));
        my $iSt = 0;
        while($iSt < $iLen) {
            my($iOpW, $iWkS, $iRs, $iRe, $iCs, $iCe) = 
                unpack('cv5', substr($sObj, $iSt+3, 11));

            if($iOpW == 0x3b) {
                $iSheet = $iWkS;
                push @aRes, [$iRs, $iCs, $iRe, $iCe];
            }

            if($iSt==0) {
                $iSt += 11;
            }
            else {
                $iSt += 12; #Skip 1 byte;
            }
        }
    }
    return ($iSheet, \@aRes);
}
#------------------------------------------------------------------------------
# ParseNameArea95 (for Spreadsheet::ParseExcel) DK: 494 (ptgAread3d)
#------------------------------------------------------------------------------
sub _ParseNameArea95 {
    my ($sObj) =@_;
    my ($iOp);
    my @aRes = ();
    $iOp = unpack('C', $sObj);
    my $iSheet;
    if($iOp == 0x3b) {
        $iSheet = unpack('v', substr($sObj, 11, 2));
        my($iRs, $iRe, $iCs, $iCe) = 
                unpack('v2C2', substr($sObj, 15, 6));
        push @aRes, [$iRs, $iCs, $iRe, $iCe];
    }
    elsif($iOp == 0x29) {
        my $iLen = unpack('v', substr($sObj, 1, 2));
        my $iSt = 0;
        while($iSt < $iLen) {
            my $iOpW = unpack('c', substr($sObj, $iSt+3, 6));
            $iSheet = unpack('v', substr($sObj, $iSt+14, 2));
            my($iRs, $iRe, $iCs, $iCe) = 
                unpack('v2C2', substr($sObj, $iSt+18, 6));
            push @aRes, [$iRs, $iCs, $iRe, $iCe] if($iOpW == 0x3b);

            if($iSt==0) {
                $iSt += 21;
            }
            else {
                $iSt += 22; #Skip 1 byte;
            }
        }
    }
    return ($iSheet, \@aRes);
}
#------------------------------------------------------------------------------
# _subBOOL (for Spreadsheet::ParseExcel) DK: P452
#------------------------------------------------------------------------------
sub _subWSBOOL {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    $oBook->{Worksheet}[$oBook->{_CurSheet}]->{PageFit} = 
                                ((unpack('v', $sWk) & 0x100)? 1: 0);
}
#------------------------------------------------------------------------------
# _subMergeArea (for Spreadsheet::ParseExcel) DK: (Not)
#------------------------------------------------------------------------------
sub _subMergeArea {
    my($oBook, $bOp, $bLen, $sWk) = @_;
    return undef unless(defined $oBook->{_CurSheet});

    my $iCnt = unpack("v", $sWk);
    my $oWkS = $oBook->{Worksheet}[$oBook->{_CurSheet}];
    $oWkS->{MergedArea} = [] unless(defined $oWkS->{MergedArea});
    for(my $i=0; $i < $iCnt; $i++) {
        my($iRs, $iRe, $iCs, $iCe) = unpack('v4', substr($sWk, $i*8 + 2, 8));
        for(my $iR=$iRs;$iR<=$iRe;$iR++) {
            for(my $iC=$iCs;$iC<=$iCe;$iC++) {
                $oWkS->{Cells}[$iR][$iC] ->{Merged} = 1 
                        if(defined $oWkS->{Cells}[$iR][$iC] );
            }
        }
        push @{$oWkS->{MergedArea}}, [$iRs, $iCs, $iRe, $iCe];
    }
}
#------------------------------------------------------------------------------
# DecodeBoolErr (for Spreadsheet::ParseExcel) DK: P306
#------------------------------------------------------------------------------
sub DecodeBoolErr {
    my($iVal, $iFlg) = @_;
    if($iFlg) {     # ERROR
        if($iVal == 0x00) {
            return "#NULL!";
        }
        elsif($iVal == 0x07) {
            return "#DIV/0!";
        }
        elsif($iVal == 0x0F) {
            return "#VALUE!";
        }
        elsif($iVal == 0x17) {
            return "#REF!";
        }
        elsif($iVal == 0x1D) {
            return "#NAME?";
        }
        elsif($iVal == 0x24) {
            return "#NUM!";
        }
        elsif($iVal == 0x2A) {
            return "#N/A!";
        }
        else {
            return "#ERR";
        }
    }
    else {
        return ($iVal)? "TRUE" : "FALSE";
    }
}
#------------------------------------------------------------------------------
# _UnpackRKRec (for Spreadsheet::ParseExcel)    DK:P 401
#------------------------------------------------------------------------------
sub _UnpackRKRec {
    my($sArg) = @_;

    my $iF  = unpack('v', substr($sArg, 0, 2));

    my $lWk = substr($sArg, 2, 4);
    my $sWk = pack("c4", reverse(unpack("c4", $lWk)));
    my $iPtn = unpack("c",substr($sWk, 3, 1)) & 0x03;
    if($iPtn == 0) {
        return ($iF, unpack("d", ($BIGENDIAN)? $sWk . "\0\0\0\0": "\0\0\0\0". $lWk));
    }
    elsif($iPtn == 1) {
        # http://rt.cpan.org/Ticket/Display.html?id=18063
        my $u31 = unpack("c",substr($sWk, 3, 1)) & 0xFC;
        $u31 |= 0xFFFFFF00 if ($u31 & 0x80); # raise neg bits for neg 1-byte value
        substr($sWk, 3, 1) &= pack('c', $u31);

        my $u01 = unpack("c",substr($lWk, 0, 1)) & 0xFC;
        $u01 |= 0xFFFFFF00 if ($u01 & 0x80); # raise neg bits for neg 1-byte value
        substr($lWk, 0, 1) &= pack('c', $u01);

        return ($iF, unpack("d", ($BIGENDIAN)? $sWk . "\0\0\0\0": "\0\0\0\0". $lWk)/ 100);
    }
    elsif($iPtn == 2) {
    my $sUB = unpack("B32", $sWk);
        my $sWkLB = pack("B32", (substr($sUB, 0, 1) x 2) .
                                substr($sUB, 0, 30));
        my $sWkL  = ($BIGENDIAN)? $sWkLB: pack("c4", reverse(unpack("c4", $sWkLB)));
        return ($iF, unpack("i", $sWkL));
    }
    else {
    my $sUB = unpack("B32", $sWk);
        my $sWkLB = pack("B32", (substr($sUB, 0, 1) x 2) .
                                substr($sUB, 0, 30));
        my $sWkL  = ($BIGENDIAN)? $sWkLB: pack("c4", reverse(unpack("c4", $sWkLB)));
        return ($iF, unpack("i", $sWkL) / 100);
    }
}
#------------------------------------------------------------------------------
# _subStrWk (for Spreadsheet::ParseExcel)     DK:P280 ..
#------------------------------------------------------------------------------
sub _subStrWk {
    my($oBook, $sWk, $fCnt) = @_;

    #1. Continue
    if(defined($fCnt)) {
    #1.1 Before No Data No
        if($oBook->{StrBuff} eq '') { #
#print "CONT NO DATA\n";
#print "DATA:", unpack('H30', $oBook->{StrBuff}), " PRE:$oBook->{_PrevCond}\n";
            $oBook->{StrBuff} .= $sWk;
        }
        #1.1 No PrevCond 
        elsif(!(defined($oBook->{_PrevCond}))) {
#print "NO PREVCOND\n";
                $oBook->{StrBuff} .= substr($sWk, 1);
        }
        else {
#print "CONT\n";
            my $iCnt1st = ord($sWk); # 1st byte of Continue may be a GR byte
            my($iStP, $iLenS) = @{$oBook->{_PrevInfo}};
            my $iLenB = length($oBook->{StrBuff});

        #1.1 Not in String
            if($iLenB >= ($iStP + $iLenS)) {
#print "NOT STR\n";
                $oBook->{StrBuff} .= $sWk;
#                $oBook->{StrBuff} .= substr($sWk, 1);
            }
        #1.2 Same code (Unicode or ASCII)
            elsif(($oBook->{_PrevCond} & 0x01) == ($iCnt1st & 0x01)) {
#print "SAME\n";
                $oBook->{StrBuff} .= substr($sWk, 1);
            }
            else {
        #1.3 Diff code (Unicode or ASCII)
                my $iDiff = ($iStP + $iLenS) - $iLenB;
                if($iCnt1st & 0x01) {
#print "DIFF ASC $iStP $iLenS $iLenB DIFF:$iDiff\n";
#print "BEF:", unpack("H6", $oBook->{StrBuff}), "\n";
                  my ($iDum, $iGr) =unpack('vc', $oBook->{StrBuff});
                  substr($oBook->{StrBuff}, 2, 1) = pack('c', $iGr | 0x01);
#print "AFT:", unpack("H6", $oBook->{StrBuff}), "\n";
                    for(my $i = ($iLenB-$iStP); $i >=1; $i--) {
                        substr($oBook->{StrBuff}, $iStP+$i, 0) =  "\x00"; 
                    }
                }
                else {
#print "DIFF UNI:", $oBook->{_PrevCond}, ":", $iCnt1st, " DIFF:$iDiff\n";
                    for(my $i = ($iDiff/2); $i>=1;$i--) {
                        substr($sWk, $i+1, 0) =  "\x00";
                    }
                }
                $oBook->{StrBuff} .= substr($sWk, 1);
           }
        }
    }
    else {
    #2. Saisho
        $oBook->{StrBuff} .= $sWk;
    }
#print " AFT2:", unpack("H60", $oBook->{StrBuff}), "\n";

    $oBook->{_PrevCond} = undef;
    $oBook->{_PrevInfo} = undef;

    while(length($oBook->{StrBuff}) >= 4) {
        my ( $raBuff, $iLen, $iStPos, $iLenS) = _convBIFF8String($oBook, $oBook->{StrBuff}, 1);
                                                    #No Code Convert
        if(defined($raBuff->[0])) {
            push @{$oBook->{PkgStr}}, 
                {
                    Text    => $raBuff->[0],
                    Unicode => $raBuff->[1],
                    Rich    => $raBuff->[2],
                    Ext     => $raBuff->[3],
            };
            $oBook->{StrBuff} = substr($oBook->{StrBuff}, $iLen);
        }
        else {
            $oBook->{_PrevCond} = $raBuff->[1];
            $oBook->{_PrevInfo} = [$iStPos, $iLenS];
            last;
        }
    }
}
#------------------------------------------------------------------------------
# _SwapForUnicode (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _SwapForUnicode {
    my($sObj) = @_;
#    for(my $i = 0; $i<length($$sObj); $i+=2){
    for(my $i = 0; $i<(int (length($$sObj) / 2) * 2); $i+=2) {
            my $sIt = substr($$sObj, $i, 1);
            substr($$sObj, $i, 1) = substr($$sObj, $i+1, 1);
            substr($$sObj, $i+1, 1) = $sIt;
    }
}
#------------------------------------------------------------------------------
# _NewCell (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub _NewCell {
    my($oBook, $iR, $iC, %rhKey)=@_;
    my($sWk, $iLen);
    return undef unless(defined $oBook->{_CurSheet});

    my $oCell = 
        Spreadsheet::ParseExcel::Cell->new(
            Val     => $rhKey{Val},
            FormatNo=> $rhKey{FormatNo},
            Format  => $rhKey{Format},
            Code    => $rhKey{Code},
            Type    => $oBook->{FmtClass}->ChkType(
                            $rhKey{Numeric}, 
                            $rhKey{Format}->{FmtIdx}),
        );
    $oCell->{_Kind}  = $rhKey{Kind};
    $oCell->{_Value} = $oBook->{FmtClass}->ValFmt($oCell, $oBook);
    if($rhKey{Rich}) {
        my @aRich = ();
        my $sRich = $rhKey{Rich};
        for(my $iWk=0;$iWk<length($sRich); $iWk+=4) {
            my($iPos, $iFnt) = unpack('v2', substr($sRich, $iWk));
            push @aRich, [$iPos, $oBook->{Font}[$iFnt]];
        }
        $oCell->{Rich}   =  \@aRich;
    }

    if(defined $_CellHandler) {
        if(defined $_Object){
            no strict;
            ref($_CellHandler) eq "CODE" ? 
                    $_CellHandler->($_Object, $oBook, $oBook->{_CurSheet}, $iR, $iC, $oCell) :
                    $_CellHandler->callback($_Object, $oBook, $oBook->{_CurSheet}, $iR, $iC, $oCell);
        }
        else{
            $_CellHandler->($oBook, $oBook->{_CurSheet}, $iR, $iC, $oCell);
        }
    }
    unless($_NotSetCell) {
        $oBook->{Worksheet}[$oBook->{_CurSheet}]->{Cells}[$iR][$iC] 
            = $oCell;
    }
    return $oCell;
}
#------------------------------------------------------------------------------
# ColorIdxToRGB (for Spreadsheet::ParseExcel)
#------------------------------------------------------------------------------
sub ColorIdxToRGB {
    my($sPkg, $iIdx) = @_;
    return ((defined $aColor[$iIdx])? $aColor[$iIdx] : $aColor[0]);
}

#DESTROY {
#    my ($self) = @_;
#    warn "DESTROY $self called\n"
#}

1;
__END__

=head1 NAME

Spreadsheet::ParseExcel - Get information from Excel file

=head1 SYNOPSIS

    
I<new interface>

    use strict;
    use Spreadsheet::ParseExcel;

    my $excel = Spreadsheet::ParseExcel::Workbook->Parse($file);
    foreach my $sheet (@{$excel->{Worksheet}}) {
        printf("Sheet: %s\n", $sheet->{Name});
        $sheet->{MaxRow} ||= $sheet->{MinRow};
        foreach my $row ($sheet->{MinRow} .. $sheet->{MaxRow}) {
            $sheet->{MaxCol} ||= $sheet->{MinCol};
            foreach my $col ($sheet->{MinCol} ..  $sheet->{MaxCol}) {
                my $cell = $sheet->{Cells}[$row][$col];
                if ($cell) {
                    printf("( %s , %s ) => %s\n", $row, $col, $cell->{Val});
                }
            }
        }
    }


I<old interface>
    use strict;
    use Spreadsheet::ParseExcel;
    my $oExcel = Spreadsheet::ParseExcel->new;

    #1.1 Normal Excel97
    my $oBook = $oExcel->Parse('Excel/Test97.xls');
    my($iR, $iC, $oWkS, $oWkC);
    print "FILE  :", $oBook->{File} , "\n";
    print "COUNT :", $oBook->{SheetCount} , "\n";
    print "AUTHOR:", $oBook->{Author} , "\n";
    for(my $iSheet=0; $iSheet < $oBook->{SheetCount} ; $iSheet++) {
        $oWkS = $oBook->{Worksheet}[$iSheet];
        print "--------- SHEET:", $oWkS->{Name}, "\n";
        for(my $iR = $oWkS->{MinRow} ; 
                defined $oWkS->{MaxRow} && $iR <= $oWkS->{MaxRow} ; $iR++) {
            for(my $iC = $oWkS->{MinCol} ;
                            defined $oWkS->{MaxCol} && $iC <= $oWkS->{MaxCol} ; $iC++) {
                $oWkC = $oWkS->{Cells}[$iR][$iC];
                print "( $iR , $iC ) =>", $oWkC->Value, "\n" if($oWkC);  # Formatted Value
                print "( $iR , $iC ) =>", $oWkC->{Val}, "\n" if($oWkC);  # Original Value
            }
        }
    }

=head1 DESCRIPTION

Spreadsheet::ParseExcel makes you to get information from Excel95, Excel97, Excel2000 file.

=head2 Functions

=over 4

=item new

I<$oExcel> = Spreadsheet::ParseExcel->new(
                    [ I<CellHandler> => \&subCellHandler, 
                      I<NotSetCell> => undef | 1,
                    ]);

Constructor.


=over 4

=item CellHandler I<(experimental)>

specify callback function when a cell is detected.

I<subCellHandler> gets arguments like below:

sub subCellHandler (I<$oBook>, I<$iSheet>, I<$iRow>, I<$iCol>, I<$oCell>);

B<CAUTION> : The atributes of Workbook may not be complete.
This function will be called almost order by rows and columns.
Take care B<almost>, I<not perfectly>.

=item NotSetCell I<(experimental)>

specify set or not cell values to Workbook object.

=back

=item Parse

I<$oWorkbook> = $oParse->Parse(I<$sFileName> [, I<$oFmt>]);

return L<"Workbook"> object.
if error occurs, returns undef.

=over 4

=item I<$sFileName>

name of the file to parse

From 0.12 (with OLE::Storage_Lite v.0.06), 
scalar reference of file contents (ex. \$sBuff) or 
IO::Handle object (inclucdng IO::File etc.) are also available.

=item I<$oFmt>

L<"Formatter Class"> to format the value of cells.

=back

=item ColorIdxToRGB

I<$sRGB> = $oParse->ColorIdxToRGB(I<$iColorIdx>);

I<ColorIdxToRGB> returns RGB string corresponding to specified color index.
RGB string has 6 charcters, representing RGB hex value. (ex. red = 'FF0000')

=back

=head2 Workbook

I<Spreadsheet::ParseExcel::Workbook>

Workbook class has these methods :

=over 4

=item Parse

(class method) : same as Spreadsheet::ParseExcel

=back

=over 4

=item Worksheet

I<$oWorksheet> = $oBook->Worksheet(I<$sName>);

I<Worksheet> returns a Worksheet object with I<$sName> or undef.
If there is no worksheet with I<$sName> and I<$sName> contains only digits,
it returns a Worksheet object at that position.

=back

Workbook class has these properties :

=over 4

=item File

Name of the file

=item Author

Author of the file

=item Flg1904

If this flag is on, date of the file count from 1904.

=item Version

Version of the file

=item SheetCount

Numbers of L<"Worksheet"> s in that Workbook

=item Worksheet[SheetNo]

Array of L<"Worksheet">s class

=item PrintArea[SheetNo]

Array of PrintArea array refs.

Each PrintArea is : [ I<StartRow>, I<StartColumn>, I<EndRow>, I<EndColumn>]

=item PrintTitle[SheetNo]

Array of PrintTitle hash refs.

Each PrintTitle is : 
        { Row => [I<StartRow>, I<EndRow>], 
          Column => [I<StartColumn>, I<EndColumn>]}

=back

=head2 Worksheet

I<Spreadsheet::ParseExcel::Worksheet>

Worksheet class has these methods:

=over 4

=item Cell ( ROW, COL )

Return the Cell iobject at row ROW and column COL if
it is defined. Otherwise return undef.

=item RowRange ()

Return a two-element list (MIN, MAX) containing the
minimum and maximum of defined rows in the worksheet
If there is no row defined MAX is smaller than MIN.

=item ColRange ()

Return a two-element list (MIN, MAX) containing the
minimum and maximum of defined columns in the worksheet
If there is no row defined MAX is smaller than MIN.

=back

Worksheet class has these properties:

=over 4

=item Name

Name of that Worksheet

=item DefRowHeight

Default height of rows

=item DefColWidth

Default width of columns

=item RowHeight[Row]

Array of row height

=item ColWidth[Col]

Array of column width (undef means DefColWidth)

=item Cells[Row][Col]

Array of L<"Cell">s infomation in the worksheet

=item Landscape

Print in horizontal(0) or vertical (1).

=item Scale

Print scale.

=item FitWidth

Number of pages with fit in width. 

=item FitHeight

Number of pages with fit in height.

=item PageFit

Print with fit (or not).

=item PaperSize

Papar size. The value is like below:

  Letter               1, LetterSmall          2, Tabloid              3 ,
  Ledger               4, Legal                5, Statement            6 ,
  Executive            7, A3                   8, A4                   9 ,
  A4Small             10, A5                  11, B4                  12 ,
  B5                  13, Folio               14, Quarto              15 ,
  10x14               16, 11x17               17, Note                18 ,
  Envelope9           19, Envelope10          20, Envelope11          21 ,
  Envelope12          22, Envelope14          23, Csheet              24 ,
  Dsheet              25, Esheet              26, EnvelopeDL          27 ,
  EnvelopeC5          28, EnvelopeC3          29, EnvelopeC4          30 ,
  EnvelopeC6          31, EnvelopeC65         32, EnvelopeB4          33 ,
  EnvelopeB5          34, EnvelopeB6          35, EnvelopeItaly       36 ,
  EnvelopeMonarch     37, EnvelopePersonal    38, FanfoldUS           39 ,
  FanfoldStdGerman    40, FanfoldLegalGerman  41, User                256

=item PageStart

Start page number.

=item UsePage

Use own start page number (or not).

=item LeftMergin, RightMergin, TopMergin, BottomMergin, HeaderMergin, FooterMergin

Mergins for left, right, top, bottom, header and footer.

=item HCenter

Print in horizontal center (or not)

=item VCenter

Print in vertical center  (or not)

=item Header

Content of print header.
Please refer Excel Help.

=item Footer

Content of print footer.
Please refer Excel Help.

=item PrintGrid

Print with Gridlines (or not)

=item PrintHeaders

Print with headings (or not)

=item NoColor

Print in black-white (or not).

=item Draft

Print in draft mode (or not).

=item Notes

Print with notes (or not).

=item LeftToRight

Print left to right(0) or top to down(1).

=item HPageBreak

Array ref of horizontal page breaks.

=item VPageBreak

Array ref of vertical page breaks.

=item MergedArea

Array ref of merged areas.
Each merged area is : [ I<StartRow>, I<StartColumn>, I<EndRow>, I<EndColumn>]

=back

=head2 Cell

I<Spreadsheet::ParseExcel::Cell>

Cell class has these properties:

=over 4

=item Value

I<Method>
Formatted value of that cell

=item Val

Original Value of that cell

=item Type

Kind of that cell ('Text', 'Numeric', 'Date')

=item Code

Character code of that cell (undef, 'ucs2', '_native_')
undef tells that cell seems to be ascii.
'_native_' tells that cell seems to be 'sjis' or something like that.

=item Format

L<"Format"> for that cell.

=item Merged

That cells is merged (or not).

=item Rich

Array ref of font informations about each characters.

Each entry has : [ I<Start Position>, I<Font Object>]

For more information please refer sample/dmpExR.pl

=back

=head2 Format

I<Spreadsheet::ParseExcel::Format>

Format class has these properties:

=over 4

=item Font

L<"Font"> object for that Format.

=item AlignH

Horizontal Alignment.

  0: (standard), 1: left,       2: center,     3: right,      
  4: fill ,      5: justify,    7:equal_space  

B<Notice:> 6 may be I<merge> but it seems not to work.

=item AlignV

Vertical Alignment.

    0: top,  1: vcenter, 2: bottom, 3: vjustify, 4: vequal_space

=item Indent

Number of indent

=item Wrap

Wrap (or not).

=item Shrink

Display in shrinking (or not)

=item Rotate

In Excel97, 2000      : degrees of string rotation.
In Excel95 or earlier : 0: No rotation, 1: Top down, 2: 90 degrees anti-clockwise, 
                        3: 90 clockwise

=item JustLast

JustLast (or not).
I<I have never seen this attribute.>

=item ReadDir

Direction for read.

=item BdrStyle

Array ref of boder styles : [I<Left>, I<Right>, I<Top>, I<Bottom>]

=item BdrColor

Array ref of boder color indexes : [I<Left>, I<Right>, I<Top>, I<Bottom>]

=item BdrDiag

Array ref of diag boder kind, style and color index : [I<Kind>, I<Style>, I<Color>]
  Kind : 0: None, 1: Right-Down, 2:Right-Up, 3:Both

=item Fill

Array ref of fill pattern and color indexes : [I<Pattern>, I<Front Color>, I<Back Color>]

=item Lock

Locked (or not).

=item Hidden

Hiddedn (or not).

=item Style

Style format (or Cell format)

=back

=head2 Font

I<Spreadsheet::ParseExcel::Font>

Format class has these properties:

=over 4

=item Name

Name of that font.

=item Bold

Bold (or not).

=item Italic

Italic (or not).

=item Height

Size (height) of that font.

=item Underline

Underline (or not).

=item UnderlineStyle

0: None, 1: Single, 2: Double, 0x21: Single(Account), 0x22: Double(Account)

=item Color

Color index for that font.

=item Strikeout

Strikeout (or not).

=item Super

0: None, 1: Upper, 2: Lower

=back

=head1 Formatter class

I<Spreadsheet::ParseExcel::Fmt*>

Formatter class will convert cell data.

Spreadsheet::ParseExcel includes 2 formatter classes: FmtDefault and FmtJapanese. 
You can create your own FmtClass as you like.

Formatter class(Spreadsheet::ParseExcel::Fmt*) should provide these functions:

=over 4

=item ChkType($oSelf, $iNumeric, $iFmtIdx)

tells type of the cell that has specified value.

=over 8

=item $oSelf

Formatter itself

=item $iNumeric

If on, the value seems to be number

=item $iFmtIdx

Format index number of that cell

=back

=item TextFmt($oSelf, $sText, $sCode)

converts original text into applicatable for Value.

=over 8

=item $oSelf

Formatter itself

=item $sText

Original text

=item $sCode

Character code of Original text

=back

=item ValFmt($oSelf, $oCell, $oBook) 

converts original value into applicatable for Value.

=over 8

=item $oSelf

Formatter itself

=item $oCell

Cell object

=item $oBook

Workbook object

=back

=item FmtString($oSelf, $oCell, $oBook)

get format string for the I<$oCell>.

=over 8

=item $oSelf

Formatter itself

=item $oCell

Cell object

=item $oBook

WorkBook object contains that cell

=back

=back

=head1 KNOWN PROBLEM

This module can not get the values of fomulas in 
Excel files made with Spreadsheet::WriteExcel.
Normaly (ie. By Excel application), formula has the result with it.
But Spreadsheet::WriteExcel writes formula with no result.
If you set your Excel application "Auto Calculation" off.
(maybe [Tool]-[Option]-[Calculation] or something)
You will see the same result.

=head1 AUTHOR

Current maintainer: Gabor Szabo szabgab@cpan.org

    http://www.szabgab.com/

Original author: Kawai Takanori (Hippo2000) kwitknr@cpan.org

    http://member.nifty.ne.jp/hippo2000/            (Japanese)
    http://member.nifty.ne.jp/hippo2000/index_e.htm (English)

=head1 SEE ALSO

XLHTML, OLE::Storage, Spreadsheet::WriteExcel, OLE::Storage_Lite

This module is based on herbert within OLE::Storage and XLHTML.

XLSTools: http://perl.jonallen.info/projects/xlstools

=head1 TODO

- Add tests, and more tests

- Spreadsheet::ParseExcel : 
 Password protected data, Formulas support, HyperLink support, 
 Named Range support

- Spreadsheet::ParseExcel::SaveParser :
 Catch up Spreadsheet::WriteExce feature, Create new Excel fle

See also:

 L<http://www.cpanforum.com/dist/Spreadsheet-ParseExcel>

 and

 http://www.perlmonks.org/index.pl?node_id=490656
 http://www.perlmonks.org/index.pl?node_id=379743 
 http://www.perlmonks.org/index.pl?node_id=433192
 http://www.perlmonks.org/index.pl?node_id=422147



=head1 COPYRIGHT

Copyright (c) 2006-2007 Gabor Szabo
Copyright (c) 2000-2006 Kawai Takanori
All rights reserved.

You may distribute under the terms of either the GNU General Public
License or the Artistic License, as specified in the Perl README file.

=head1 ACKNOWLEDGEMENTS

First of all, I would like to acknowledge valuable program and modules :
XHTML, OLE::Storage and Spreadsheet::WriteExcel.

In no particular order: Yamaji Haruna, Simamoto Takesi, Noguchi Harumi, 
Ikezawa Kazuhiro, Suwazono Shugo, Hirofumi Morisada, Michael Edwards, 
Kim Namusk, Slaven Rezić, Grant Stevens, 
and many many people + Kawai Mikako.

=cut
