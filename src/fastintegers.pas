unit FastIntegers;
// Copyright 2022-2024 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{$mode objfpc}{$H+}

interface

type

  Int64Fast = Int64;
  UInt64Fast = UInt64;

  Int32Fast = Int32; //{$if SizeOf(SizeInt) <= 4}Int32{$else}SizeInt{$endif};
  UInt32Fast = UInt32; //{$if SizeOf(SizeInt) <= 4}UInt32{$else}SizeUInt{$endif};

  Int16Fast = {$if SizeOf(SizeInt) <= 2}Int16{$else}Int32Fast{$endif};
  UInt16Fast = {$if SizeOf(SizeInt) <= 2}UInt16{$else}UInt32Fast{$endif};

  Int8Fast = {$if SizeOf(SizeInt) <= 1}Int8{$else}Int16Fast{$endif};
  UInt8Fast = {$if SizeOf(SizeInt) <= 1}UInt8{$else}UInt16Fast{$endif};

implementation

end.

