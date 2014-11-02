#!/usr/bin/env bash

bake_task install_cider "Install Emacs CIDER"
function install_cider () {
  pushd software
  if [ ! -d cider ]; then
    git clone $CIDER_GIT_URL
  fi

  pushd cider
  git checkout master
  git pull origin master
  git checkout $CIDER_GIT_TAG
  popd
  popd
}

bake_task install_ac_cider "Install Emacs auto completion for Cider"
function install_ac_cider () {
  pushd software
  if [ ! -d ac-cider ]; then
    git clone "$AC_CIDER_GIT_URL"
  fi

  pushd ac-cider
  git checkout master
  git pull origin master
  git checkout "$AC_CIDER_GIT_TAG"
  popd
  popd
}

