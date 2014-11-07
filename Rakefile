require 'rake'
require 'erb'

# Installs things that fresh cannot handle because they need to be built
# OhMyZSH for shell
# Janus for Vim
# Bashit for bash
# Terminal Profiles


desc "install the dot files into user's home directory"
task :default do

  switch_to_zsh
  install_bashit
  install_janus
  terminal_profiles
  xiki_rvm
  atom_plugins
end



def atom_plugins
  unless File.exist?(File.join(ENV['HOME'], ".atom"))
    puts "atom not installed"
    exit
  end

  print "install atom plugins? (recommended) [ynq] "
  case $stdin.gets.chomp
  when 'y'
    puts "importing text file"
    system %Q{apm install --packages-file ~/.dotfiles/config/atom/atom-packages.json}
  when 'q'
    exit
  else
    puts "skipping atom plugins"
  end

end

def terminal_profiles
  unless File.exist?(File.join(ENV['HOME'], ".fresh/build/gnome-terminal.xml"))
    puts "no profiles to import"
    exit
  end

  print "install terminal profiles? (recommended) [ynq] "
  case $stdin.gets.chomp
  when 'y'
    puts "importing xml"
    system %Q{gconftool-2 --load ~/.fresh/build/gnome-terminal-conf.xml}
  when 'q'
    exit
  else
    puts "skipping terminal profiles"
  end

end

def switch_to_zsh
  if ENV["SHELL"] =~ /zsh/
    puts "using zsh"
  else
    print "switch to zsh? (recommended) [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "switching to zsh"
      system %Q{chsh -s `which zsh`}
    when 'q'
      exit
    else
      puts "skipping zsh"
    end
  end
end



def install_bashit
  if File.exist?(File.join(ENV['HOME'], ".bash_it"))
    puts "found ~/.bash_it"
    system %Q{upgrade_bashit}
  else
    print "install bashit? [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "installing bashit"
      system %Q{git clone https://github.com/revans/bash-it.git "$HOME/.bash_it"}
      system %Q{sh '~/.bash_it/install.sh'}
    when 'q'
      exit
    else
      puts "skipping bashit, you will need to change ~/.bash_profile"
    end
  end
end



def xiki_rvm
  if File.exist?(File.join(ENV['HOME'], ".rvm"))
    puts "not using rvm, quitting"
    exit
  end

  print "install xiki? (recommended) [ynq] "
  case $stdin.gets.chomp
  when 'y'
    puts "running bundle"
    system %Q{cd ~/.src/xiki/}
    system %Q{gem install bundler}
    system %Q{bundle}
    system %Q{ruby etc/command/copy_xiki_command_to.rb /usr/local/bin/xiki}
  when 'q'
    exit
  else
    puts "skipping xiki"
  end

end

def install_janus
  if File.exist?(File.join(ENV['HOME'], ".vim"))
    puts "found ~/.vim, please run rake inside it"
  else
    print "install janus? [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "installing janus"
      system %Q{curl -Lo- https://bit.ly/janus-bootstrap | bash}
    when 'q'
      exit
    else
      puts "skipping janus"
    end
  end
end
