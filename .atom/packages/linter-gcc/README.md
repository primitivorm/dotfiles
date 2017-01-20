# linter-gcc
[![Travis](https://img.shields.io/travis/hebaishi/linter-gcc.svg?style=flat-square)](https://travis-ci.org/hebaishi/linter-gcc) [![apm](https://img.shields.io/apm/dm/linter-gcc.svg?style=flat-square)](https://atom.io/packages/linter-gcc)
[![Paypal](https://www.paypalobjects.com/en_GB/i/btn/btn_donate_SM.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=44GUEE2BT7XVG)

Linter plugin for [Linter](https://github.com/AtomLinter/Linter), provides an interface to gcc/g++.

Used with files with grammar "C", "C++" and ["C++14"](https://atom.io/packages/language-cpp14).

Now with linting **on-the-fly**! This is a new feature so please open an issue if you encounter any problems.

## Important info for Mac OSX users!
If you have XCode installed on OSX, the `gcc/g++` commands will both link to `clang`. This can cause issues with the `-fmax-errors` option used by linter-gcc, which isn't recognised by clang. To properly install GCC, you need to install it with Homebrew (instructions [here](https://github.com/hebaishi/linter-gcc/issues/62)).

## Linter in action!

![linter-gcc screenshot](https://raw.githubusercontent.com/hebaishi/images/master/lintergcc_onthefly.gif)

## Using CMake compile settings
linter-gcc can take compile settings from CMake. Using my gtf2tab project as an example, this is what you need to do:

```bash
git clone https://github.com/hebaishi/gtf2tab
cd gtf2tab
mkdir build
cd build
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
```

Running ```cmake``` with the ```-DCMAKE_EXPORT_COMPILE_COMMANDS``` flag generates a ```compile_commands.json``` file which linter-gcc can get the compile settings from. Then you simply open the project in Atom, and enter ```./build/compile_commands.json``` in the Compile Commands File setting of linter-gcc. Note that if you supply a valid ```compile_commands.json``` file, your include paths and compile flags configuration settings (described below) are ignored.

## File/Project-Specific settings

Assuming you have the a file called ```sample.cpp``` open, linter-gcc performs the following actions:

1. Looks for file called ```sample.cpp.gcc-flags.json``` in the same directory as your source file (file-specific settings)
2. Looks for a file called ```.gcc-flags.json``` in every subdirectory from the current up to your project root (subdirectory/project-specific settings)
3. If no ```.gcc-flags.json``` is found, the settings in your configuration page are used.

The package takes its settings from the first configuration file that is found.

You can specify your settings in ```.gcc-flags.json```, at any level (file/subdirectory/project) using the following syntax:

```json
{
  "execPath": "/usr/bin/g++",
  "gccDefaultCFlags": "-Wall",
  "gccDefaultCppFlags": "-Wall -std=c++11",
  "gccErrorLimit": 15,
  "gccIncludePaths": ".,./include,./path",
  "gccSuppressWarnings": true
}
```

Note that the include paths need to be separated by commas. If this file is present, it will replace the settings you specified in the settings window. Relative paths (starting with ```.``` or ```..```) are expanded with respect to the root folder. Both ```execPath``` and ```gccIncludePaths``` are expanded.

In order to avoid unwanted behavior associated with having multiple projects open, only the paths within the first project are used, and the package limits its search to 30 levels when looking for a configuration file. You can work with multiple projects, as long as each is open in a separate window. Additionally, within each project, you may have as many file/directory-specific configuration files as you wish.

### Usage notes:
* Add ```-fsyntax-only``` to your C/C++ compilation flags to prevent the generation of ```.gch``` files when linting headers
* Add ```-c``` to your flags to avoid linking errors.

### Plugin installation
Press ctrl and ',' or cmd and ',' , click on 'Packages', search 'linter gcc', or:
```
$ apm install linter-gcc
```
### Reporting Issues

Please read the [Wiki](https://github.com/hebaishi/linter-gcc/wiki/) before reporting any issues.
