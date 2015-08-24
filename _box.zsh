#compdef box

local cmdline

index=$((CURRENT - 1))
request=(--bash-completion-index $index)
for arg in ${words[@]}; do
  request=(${request[@]} --bash-completion-word $arg)
done

compadd -- $( box "${request[@]}" )
