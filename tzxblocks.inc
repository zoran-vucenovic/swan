
// Copyright 2022 Zoran Vučenović
// SPDX-License-Identifier: Apache-2.0

{
  Tzx blocks specification: https://worldofspectrum.net/TZXformat.html
}
                               
{$i tzxblockStandard.inc}
{$i tzxblock11abstract.inc}
{$i tzxblock10.inc} // Standard Speed Data Block
{$i tzxblock11.inc} // Turbo Speed Data Block
{$i tzxblock12.inc} // Pure Tone
{$i tzxblock13.inc} // Pulse sequence
{$i tzxblock14.inc} // Pure Data Block
{$i tzxblock15.inc} // Direct Recording -- not tested, cannot find any tzx file with block 15?!
{$i tzxblock19.inc} // Generalized Data Block
{$i tzxblock20.inc} // Pause (silence) or 'Stop the Tape' command
{$i tzxblock21.inc} // Group start
{$i tzxblock22.inc} // Grop end
{$i tzxblock23.inc} // Jump to block
{$i tzxblock24.inc} // Loop start
{$i tzxblock25.inc} // Loop end
{$i tzxblock26.inc} // Call sequence
{$i tzxblock27.inc} // Return from sequence
{$i tzxblock28.inc} // Select block -- not implemented, cannot find any tzx file with block 28?!
{$i tzxblock2A.inc} // Stop the tape if in 48K mode
{$i tzxblock2B.inc} // Set signal level
{$i tzxblock30.inc} // Text description
{$i tzxblock31.inc} // Message block
{$i tzxblock32.inc} // Archive info
{$i tzxblock33.inc} // Hardware type
{$i tzxblock34.inc} // Emulation info
{$i tzxblock35.inc} // Custom info block
{$i tzxblock40.inc} // Snapshot block, not tested
{$i tzxblock5a.inc} // Glue block

{$i tzxblockunsupported.inc}
                        

{$i tzxblock16.inc} // C64 ROM Type Data Block
{$i tzxblock17.inc} // C64 Turbo Tape Data Block
{$i tzxblock18.inc} // CSW Recording

