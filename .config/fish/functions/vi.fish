function vi
  if not pgrep -f emacs > /dev/null
    command emacs -create-frame
  else
    command emacs 
  end
end


#  if test -n "$NVIM_LISTEN_ADDRESS" 
#    nvr $argv
#  else 
#    nvim $argv
#  end