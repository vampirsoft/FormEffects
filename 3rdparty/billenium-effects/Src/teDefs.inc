
{$ifdef VER130}
  {$ifdef BCB}
    {$define C5} // C++ Builder 5
  {$else}
    {$define D5} // Delphi 5
  {$endif BCB}
{$endif VER130}

{$ifdef VER140}
  {$ifdef BCB}
    {$define C6} // C++ Builder 6
  {$else}
    {$define D6} // Delphi 6
  {$endif BCB}
{$endif VER140}

{$ifdef VER150}
  {$define D7} // Delphi 7
{$endif VER150}

{$ifdef VER170}
  {$define D9} // Delphi 2005
{$endif VER170}

{$ifdef VER180}
  {$define D10} // Delphi 2006
{$endif VER180}

{$ifdef VER185}
  {$define D11} // Delphi 2007
{$endif VER185}

{$ifdef VER200}
  {$define D12} // Delphi 2009
{$endif VER200}

{$ifdef VER210}
  {$define D14} // Delphi 2010
{$endif VER210}

{$ifdef VER220}
  {$define D15} // Delphi XE
{$endif VER220}

{$ifdef VER350} // Delphi 11
  {$define D15}
{$endif VER350}

{$ifdef VER360} // Delphi 12
  {$define D15}
{$endif VER350}

{$ifdef D6}
  {$define D6UP} // Delphi 6 or superior
  {$undef D6}
{$endif D6}

{$ifdef C6}
  {$define D6UP} // Delphi 6 or superior
{$endif C6}
{$ifdef D7}
  {$define D6UP} // Delphi 6 or superior
  {$define D7UP} // Delphi 7 or superior
  {$undef D7}
{$endif D7}

{$ifdef D9}
  {$define D6UP} // Delphi 6 or superior
  {$define D7UP} // Delphi 7 or superior
  {$define D9UP} // Delphi 9 or superior
  {$undef D9}
{$endif D9}

{$ifdef D10}
  {$define D6UP}  // Delphi 6 or superior
  {$define D7UP}  // Delphi 7 or superior
  {$define D9UP}  // Delphi 2005 or superior
  {$define D10UP} // Delphi 2006 or superior
  {$undef D10}
{$endif D10}

{$ifdef D11}
  {$define D6UP}  // Delphi 6 or superior
  {$define D7UP}  // Delphi 7 or superior
  {$define D9UP}  // Delphi 2005 or superior
  {$define D10UP} // Delphi 2006 or superior
  {$define D11UP} // Delphi 2007 or superior
  {$undef D11}
{$endif D11}

{$ifdef D12}
  {$define D6UP}  // Delphi 6 or superior
  {$define D7UP}  // Delphi 7 or superior
  {$define D9UP}  // Delphi 2005 or superior
  {$define D10UP} // Delphi 2006 or superior
  {$define D11UP} // Delphi 2007 or superior
  {$define D12UP} // Delphi 2009 or superior
  {$undef D12}
{$endif D12}

{$ifdef D14}
  {$define D6UP}  // Delphi 6 or superior
  {$define D7UP}  // Delphi 7 or superior
  {$define D9UP}  // Delphi 2005 or superior
  {$define D10UP} // Delphi 2006 or superior
  {$define D11UP} // Delphi 2007 or superior
  {$define D12UP} // Delphi 2009 or superior
  {$define D14UP} // Delphi 2010 or superior
  {$undef D14}
{$endif D14}

{$ifdef D15}
  {$define D6UP}  // Delphi 6 or superior
  {$define D7UP}  // Delphi 7 or superior
  {$define D9UP}  // Delphi 2005 or superior
  {$define D10UP} // Delphi 2006 or superior
  {$define D11UP} // Delphi 2007 or superior
  {$define D12UP} // Delphi 2009 or superior
  {$define D14UP} // Delphi 2010 or superior
  {$undef D15}
{$endif D15}

