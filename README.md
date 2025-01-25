
# Swan

### ZX Spectrum emulator
For Windows and Linux. Might compile and run on Mac, but not tested.

The latest released version can be downloaded here: https://github.com/zoran-vucenovic/swan/releases/latest

To have sound in Swan you need free portaudio library (http://www.portaudio.com/).
Without portaudio library Swan will work, but will not produce any sound.
- In **Windows**, you can put the portaudio dll in the same folder where Swan.exe is. You can find prebuild dll for example here: https://github.com/spatialaudio/portaudio-binaries
**Important:** You have to choose the correct bitness (32-bit or 64-bit -- must be the same bitness as swan.exe)!
- In **Linux**, install libportaudio from your Linux distro's repository.

### How to compile (Windows and Linux):
1. Install Lazarus IDE (https://www.lazarus-ide.org/)
2. Download BGRABitmap graphic library and open the package bgrabitmappack.lpk in Lazarus:
   - Download BGRABitmap (https://github.com/bgrabitmap/bgrabitmap/)
   - Start Lazarus and in main menu choose **Package->Open Package File (.lpk) ...**, then go to folder where you downloaded BGRABitmap and choose bgrabitmap/bgrabitmappack.lpk and open it.
   After once opened, you can close the bgrabitmappack package, as now Lazarus knows where to find the files from this package.
3. Open swan project in Lazarus and build the project:
   - In main menu choose **Project->Open Project ...**, then open Swan.lpi.
   - Choose the build mode:
     - in main menu go to **Project->Project Options...**, then in the left side of the dialog choose **Compiler Options**. Then, on the right side of the dialog you can choose the Build mode (see more about predefined build modes below, normally just choose **Release**)
   - In main menu go to **Run->Build**

The executable file swan (swan.exe on Windows) will appear in folder **output/*platform-bitness-buildmode*/** (eg. output/x86_64-win64-Release/swan.exe)

Predefined build modes in swan project:
- Release - compile for your current platform (Windows or Linux)
- ReleaseWin64 - choose this on Windows if you have 32-bit Lazarus and want to compile for 64-bit Windows
- ReleaseQt5 - choose this if you want to compile Swan as Qt5 application on Linux in Gtk2 build of Lazarus. Keep in mind that Qt5 build needs libqt5pas library installed (see https://wiki.lazarus.freepascal.org/Qt5_Interface#libqt5pas).
- ReleaseGtk2 - choose this if you want to compile Swan as Gtk2 application on Linux in Qt build of Lazarus.
- Debug - this mode is not for building releases, it is used during development only, for debugging.
