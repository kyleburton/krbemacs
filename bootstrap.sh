#!/usr/bin/env bash

if [ -x $HOME/bin/bake ]; then
  echo "great, you already have bake!"
else
  echo -n "installing bake."
  test -d $HOME/bin || mkdir $HOME/bin
  echo -n "."
  curl https://raw.githubusercontent.com/kyleburton/bake/master/bake > $HOME/bin/bake
  echo -n "."
  chmod 755 $HOME/bin/bake
  echo -n "done!"
fi

export PATH="$PATH:$HOME/bin"
bake install_all
