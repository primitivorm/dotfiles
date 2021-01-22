# Get the source

`git clone https://github.com/vim/vim.git`

# Configure the build

Let’s run a batch file alongside the vim directory called configure.cmd
Update the directories if necessary

You can find more options in vim/src/Make_mvc.mak. In addition to Python, you can link with Python3 (but not both), Ruby, Perl, and more.

CPU specifies your target architecture, and TOOLCHAIN specifies how to compile:

```
For a 32-bit version: CPU=I386 and TOOLCHAIN=x86
For a 64-bit version: CPU=AMD64 and TOOLCHAIN=x86_amd64
```

You can build either version, but the 64-bit version will only run on a 64-bit machine. It’ll be capable of handling multi-gigabyte files, which is nice, and may run slightly faster. (On a 64-bit machine, you could also use the native amd64 toolchain if your version of VS has it, but it really doesn’t make a difference.)

If you link to a third-party library like Python, be sure you have the appropriate version (32-bit or 64-bit).

# Build Vim

Copy file Win32.Mak to SDK_INCLUDE_DIR (C:\Program Files (x86)\Windows Kits\10\Include)

Now run build.cmd alongside the vim directory

Invoke configure once, then invoke build to compile vim.exe or gvim.exe, depending on the value of GUI.

Any time you change the values in configure.cmd and re-run configure, run `build clean` before building again.

# Install Vim

First, you need to copy the executables, docs, etc. to a directory named vim82. (The numbers should match the output of vim --version.) Run a script named copy-vim.cmd

copy-vim will place everything you need in vim82. You can stop here, if you’d like.

Copy vim82 to C:\Program Files or where you desire

vim82\install.exe will run a command-line "installer". Use the given numbers to disable actions you don’t want—I disabled 10 (create startup file), 14 (context menu), and 17–19 (desktop icons). Enter d to perform the actions, and you should be all set.

If you want to install the batch files to launch Vim in the default location (C:\windows), you’ll need to run install.exe as an administrator. You can open the Command Prompt from the Start menu as an administrator by holding Ctrl+Shift, or just right-click install.exe and choose "Run as administrator". You could also install to somewhere else in your PATH.

# Troubleshooting

If you compile with python3 copy C:\Python3x\python.exe and rename to C:\Python3x\python3.exe
