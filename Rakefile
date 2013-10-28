require 'rake'
require 'erb'

desc "install the dot files into user's home directory"
task :default do
  install_oh_my_zsh
  switch_to_zsh
  install_bashit
  install_janus
  terminal_profiles
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

def install_oh_my_zsh
  if File.exist?(File.join(ENV['HOME'], ".oh-my-zsh"))
    puts "found ~/.oh-my-zsh"
    system %Q{upgrade_oh_my_zsh}
  else
    print "install oh-my-zsh? [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "installing oh-my-zsh"
      system %Q{git clone https://github.com/robbyrussell/oh-my-zsh.git "$HOME/.oh-my-zsh"}
      system %Q{cp ~/.zshrc ~/.zshrc.orig}
      system %Q{cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc}
    when 'q'
      exit
    else
      puts "skipping oh-my-zsh, you will need to change ~/.zshrc"
    end
  end
end

def install_bashit
  if File.exist?(File.join(ENV['HOME'], ".bashit"))
    puts "found ~/.bash-it"
    system %Q{upgrade_bashit}
  else
    print "install bashit? [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "installing bashit"
      system %Q{git clone https://github.com/revans/bash-it.git "$HOME/.bash-it"}
      system %Q{sh '~/.bash-it/install.sh'}
    when 'q'
      exit
    else
      puts "skipping bashit, you will need to change ~/.bash_profile"
    end
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
      system %Q{ curl -Lo- https://bit.ly/janus-bootstrap | bash}
    when 'q'
      exit
    else
      puts "skipping janus"
    end
  end
end
