
# Fancy regular
_K='\[\e[0;30m\]'
_R='\[\e[0;31m\]'
_G='\[\e[0;32m\]'
_Y='\[\e[0;33m\]'
_B='\[\e[0;34m\]'
_P='\[\e[0;35m\]'
_C='\[\e[0;36m\]'
_W='\[\e[0;37m\]'

# Fancy bold
E_K='\[\e[0;30m\]'
E_R='\[\e[0;31m\]'
E_G='\[\e[0;32m\]'
E_Y='\[\e[0;33m\]'
E_B='\[\e[0;34m\]'
E_P='\[\e[0;35m\]'
E_C='\[\e[0;36m\]'
E_W='\[\e[0;37m\]'

# Fancy background
B_K='\[\e[0;30m\]'
B_R='\[\e[0;31m\]'
B_G='\[\e[0;32m\]'
B_Y='\[\e[0;33m\]'
B_B='\[\e[0;34m\]'
B_P='\[\e[0;35m\]'
B_C='\[\e[0;36m\]'
B_W='\[\e[0;37m\]'

# Terminator
_T='\[\e[0m\]'

# PS1
export PS1="${E_R}\h:${_G}\W${_Y}"'$(__git_ps1 ".%s")'"${_T} $ "
