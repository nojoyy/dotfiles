fish_add_path /home/noah/.deno

set fonts "slant" "shadow"

set colors "red" "green" "yellow" "blue" "magenta" "cyan" 
set brcolors brred brgreen bryellow brblue brmagenta brcyan

set CFD ~/.config/fish

function cursor_up
  switch (count $argv)
    case 1
      echo -en "\e["$argv"A"
    case 0
      echo -en "\e[1A"
  end
end

function cursor_down
  switch (count $argv)
    case 1
      echo -en "\e["$argv"B"
    case 0
      echo -en "\e[1B"
  end
end

function cursor_right
  switch (count $argv)
    case 1
      echo -en "\e["$argv"C"
    case 0
      echo -en "\e[1C"
  end
end

function cursor_left
  switch (count $argv)
    case 1
      echo -en "\e["$argv"D"
    case 0
      echo -en "\e[1D"
  end
end

# Saves cursor position
function cursor_save
  echo -en "\e[s"
end

# Restores cursor position below
function cursor_restore
  echo -en "\e[u"
end

# Sets cursor, row, column
function cursor_set
  echo -en "\e["$argv[1]";"$argv[2]"H"
end

function random_index # input list size - recieve random index
  math 1 + (random) % $argv
end

function random_color
  set i (random_index (count $colors))
  echo $colors[$i]
end

function figlet_greeting
  set line (random_index (wc -l < $CFD/splash.txt)) # gets random index based off number of lines in file - < pipes the buffer from the file into the command
  set splash (sed -n $line'p' $CFD/splash.txt) # sed copies said random index from the splash file

  switch $line
    case 5
      set_color 20C20E
    case "*"
      set_color (random_color)
  end

  set font_i (random_index (count $fonts))

  set font $fonts[$font_i]

  figlet -cp -f $font $splash # uses figlet to output ascii text version
end

function neofetch_greeting
  neofetch
end

function fish_greeting
  neofetch_greeting
end

if status is-interactive
  # Commands to run in interactive sessions can go here
end

zoxide init fish | source
