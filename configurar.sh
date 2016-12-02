#########################################################
#copy configurations
#########################################################
echo "configurando sistema..."
cp -fr .dircolors ~/
cp -f .vimrc ~/
cp -f .bashrc ~/
cp -f .bash_aliases ~/
cp -f .gitconfig ~/
cp -f .profile ~/
cp -f .tmux.conf ~/

#########################################################
#install vim plugins
#########################################################
if [ ! -d ~/.vim/bundle ]; then 
    mkdir ~/.vim/bundle/
fi
if [ ! -d ~/.vim/bundle/Vundle.vim ]; then 
    echo "instalando vim plugins..."
    cd ~/.vim/bundle/
    git clone https://github.com/VundleVim/Vundle.vim
    find . -type f -exec dos2unix {} \;
    vim -c ":PluginInstall" ~/.vimrc
    find . -type f -exec dos2unix {} \;
fi
if [ ! -d ~/.vim/bundle/YouCompleteMe ]; then 
    #compile YouCompleteMe plugin
    echo "compilando YouCompleteMe plugin..."
    cd ~/.vim/bundle/
    git clone https://github.com/Valloric/YouCompleteMe
    cd ~/.vim/bundle/YouCompleteMe/
    python install.py --clang-completer
fi
cp -fr .vim ~/

#########################################################
#configure tmux
#########################################################
if [ ! -d ~/.tmuxifier ]; then 
    echo "configurando tmuxifier..."
    git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
    cd ~/.tmuxifier/
    find . -type f -exec dos2unix {} \;
    chmod +x init.sh
    ./init.sh
    cp ~/dotfiles/.dev-window.sh ~/
fi
#########################################################
#configure atom
#########################################################
if [ ! -d ~/.atom ]; then 
    echo "configurando atom..."
    bash ./usr/local/bin/atom-update
    cp -r .atom ~/.atom
fi

#########################################################
#configure powerline shell
#########################################################
#if [ ! -d ~/powerline-shell ]; then 
#    cd ~
#    git clone https://github.com/banga/powerline-shell
#    cd powerline-shell
#    cp config.py.dist config.py
#    python install.py
#    cd ~
#    ln -s ~/powerline-shell/powerline-shell.py
#    #install fonts
#    git clone https://github.com/powerline/fonts
#    cd fonts
#    ./install.sh
#    rm -fr fonts
#fi
