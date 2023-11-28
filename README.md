# Corcova
                                           
```
   _____                              
  / ____|                             
 | |     ___  _ __ ___ _____   ____ _ 
 | |    / _ \| '__/ __/ _ \ \ / / _` |
 | |___| (_) | | | (_| (_) \ V / (_| |
  \_____\___/|_|  \___\___/ \_/ \__,_|

                    λλλλ   λλλλλ         
      λλλλ        λλλλλλλλλλλλλλλλ       
  λλλλλλλλλλ     λλλλλλλλλλλλλλλλλλ      
  λλλλλλλλλλλ λλλλλλλλλλλλλλλλλλλλλλ     
     λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ   
     λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ  
      λλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλλ 
         λλλλλλλλλλλλλλλλλλλλλλλλλλλλλ   
             λλλλλλλλ         λλλλ λλλλλ 
             λλλλ λλλ         λλλλ   λλλ 
              λλλλλλλ        λλλλ    λλλ 
                 λλλλ     λλλλλ       λλ 
                 λλλ      λλλ        λλ  
```
Small CRUD in OCaml.
## Development Environment (Nix + Flakes)
```shell
$ nix develop -c $SHELL
$ dune exec ./bin/main.exe
```

## Building (Nix + Flakes)
```shell
$ nix build
```

## TODO
- [ ] Request stages (separate middleware)
- [ ] Route params

## Ideas
- Dynamic linking routes so we can add routes in runtime
- Managing database access with algebraic effects
