function mkcd
    mkdir -p -- $argv
    cd -P -- $argv
end

function gitcom
    git add . && git commit -a && git push
end

function wfrec
    wf-recorder -f ~/Desktop/recordings/(date +%Y-%m-%d-%H%M).mp4
end

function wfrecwindow
    wf-recorder -f ~/Desktop/recordings/(date +%Y-%m-%d-%H%M).mp4 -g \
	(slurp -d -b (wq color7)40 -c (wq color7) -w 3)
end

function resize4k
    convert $argv[1] -resize 4000 $argv[1]
    echo "Resized $argv[1]"
end

function tint
    convert $argv[3] -fill (wq $argv[1]) -colorize $argv[2] $argv[4]
end

function schemereload
    ~/dotfiles/scripts/schemereload.sh
end

# wayland colour picker
function clrpick
    while true;
	grim -g "$(slurp -b "00000000" -p)" - -t png -o | \
	    convert png:- -format '%[pixel:s]\n' info:- | \
	    awk -F '[(,)]' '{printf("#%02x%02x%02x\n",$2,$3,$4)}' | \
	    pastel format hex
	sleep 0.5
    end
end

# read markdown with w3m
function mdread
    pandoc $argv[1] --to html5 | w3m -T text/html
end

# pull or commit all changes to personal repos
function gitall
    if [ $argv[1] = "pull" ]
	set CWD (pwd)
	echo "dotfiles" && cd ~/dotfiles && git pull
	echo "uni" && cd ~/uni && git pull
	echo "privateconfig" && cd ~/privateconfig && git pull
	cd $CWD
    else if [ $argv[1] = "com" ]
	set CWD (pwd)
        echo "dotfiles" && cd ~/dotfiles && gitcom
        echo "uni" && cd ~/uni && gitcom
        echo "privateconfig" && cd ~/privateconfig && gitcom
        cd $CWD
    else
        echo "Requires option \"pull\" or \"com\""
    end
end
	
