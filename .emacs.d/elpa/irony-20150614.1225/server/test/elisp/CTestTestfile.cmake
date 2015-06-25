# CMake generated Testfile for 
# Source directory: C:/Users/proman02/.emacs.d/elpa/irony-20150614.1225/server/test/elisp
# Build directory: C:/Users/proman02/.emacs.d/elpa/irony-20150614.1225/server/test/elisp
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(irony-el "C:/emacs-24.5/bin/emacs.exe" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "C:/Users/proman02/.emacs.d/elpa/irony-20150614.1225/server/test/elisp/irony.el" "-f" "ert-run-tests-batch-and-exit")
add_test(irony-cdb-json-el "C:/emacs-24.5/bin/emacs.exe" "-batch" "-l" "package" "--eval" "(package-initialize) (unless (require 'cl-lib nil t) (package-refresh-contents) (package-install 'cl-lib))" "-l" "C:/Users/proman02/.emacs.d/elpa/irony-20150614.1225/server/test/elisp/irony-cdb-json.el" "-f" "ert-run-tests-batch-and-exit")
