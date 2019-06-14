function fish_user_key_bindings
        skim_key_bindings
        bind -M insert \cf forward-char
        bind -M insert \cu history-search-backward
        bind -M insert \cd history-search-forward
end
