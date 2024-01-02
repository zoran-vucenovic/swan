
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0
                               
{$ModeSwitch advancedrecords}
{$J-}
{$interfaces corba}
{$PointerMath on}

{$ifdef Debugging}
  {$Assertions on}
// When debugging, it might be useful to turn off inlining
  {$inline off}
// and then, remove the hint:
  {$WARN 3124 off : Inlining disabled}
  {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$else}
  {$Assertions off}
  {$inline on}
  {$Optimization LEVEL3}
{$endif}


