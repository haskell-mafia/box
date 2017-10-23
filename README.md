box
===

Querying and configuration-free ssh access to servers.

### Non Ambiata Note

There are Ambiata specific defaults that need to be improved
[#79](https://github.com/ambiata/box/issues/79), until then, this
project is useful in its current form via a local configuration file
(specified via BOX_FILE).


### Usage

```
# Get the address of a random server matching the filter
box ip vapour:live
box ip vapour:lab
box ip vapour:dev

# Get the address of a specific server
box ip vapour:dev:kermit.green
box ip ::kermit.green.493

# SSH to a random server matching the filter
box ssh vapour:lab

# SSH to a specific server
box ssh vapour:dev:kermit.green

# List available servers
box ls
box ls vapour
box ls :live
box ls :dev

# rsync source.txt in the current working directory to a file remote.txt
# in your home directory on vapour:dev:kermit
box rsync vapour:dev:kermit -- -aH source.txt :remote.txt

# rsync source.txt in the current working directory to an absolute file
# path /tmp/remote.txt in your home directory on vapour:dev:kermit
box rsync vapour:dev:kermit -- -aH source.txt :/tmp/remote.txt

# rsync remote.txt in your remote home directory to a file local.txt
box rsync vapour:dev:kermit -- -aH :remote.txt local.txt

# rsync an absolute directory in to a local dir
box rsync vapour:dev:kermit -- -aH :/mnt/me/plots plots

# NOTE WELL: -aH is the 'just works' options for rsync, and should be
#            all you need for almost all cases.
```

### AutoSSH

To make your ssh tunnel persistent, `box ssh` gives you an option for using `autossh.`

You will need to [install autossh locally.](https://www.everythingcli.org/ssh-tunnelling-for-fun-and-profit-autossh/)

Then use `--auto` with `box ssh.`

```
# Example usage of --auto with box ssh
box -e ci ssh --auto :gateway -- -D 6666 -N
```

### Environment Variables

|Variable           | Default              | Description |
|---                |---                   | ---         |
|`BOX_USER`         | *current unix user*  | The username used to log in to the ssh server |
|`BOX_IDENTITY`     | `~/.ssh/ambiata_rsa` | Path to the identity file used to log in to the ssh server |
|`BOX_ANSI_ESCAPES` | `1`                  | When attached to a terminal, allow the use of ANSI escape codes to change the title of the terminal, or color box's output. Set this to `0` to disable.|
|`BOX_STORE`        | `s3://ambiata-dispensary/box/v2`   | Override for when the `-e` flag doesn't work. Use `s3://ambiata-dispensary/box/*env*.v2` |

### Filters

As shown above, `box` commands accept a filter as an argument.

A filter takes one of the following forms:

```
CLIENT
CLIENT:FLAVOUR
CLIENT:FLAVOUR:NAME
```

All of the components of the filter are optional, although for most commands
you need to specify at least one of the parts. For example, you could choose
the last form, but omit the `CLIENT` and `FLAVOUR` components if you only
wanted to filter using a `NAME`:

```
::waldorf.golden
```

### Random Servers?

For `box ip` and `box ssh` we pick a random target server which matches the
filter specified. The thinking behind this is that we get a load balancing
effect, so not everyone is hammering the same server.

`box ssh` also chooses a random gateway to bounce through for the same reason.

### Advanced Usage

#### Tunnelling
Example of creating an `ssh` tunnel using `box` using port `8787` on
both local and remote ends of the tunnel.

```
box ssh vapour:dev:fozzie -- -L 8787:localhost:8787
```

### Completion Support

#### Bash

For bash completion, you need to source the `box-completion.bash` from
your `.bashrc` or `.bash_profile`.

#### Z Shell

For zsh completion, you need to copy or symlink `_box.zsh` to directory in
your `fpath` and make sure it is called `_box`.

For example, I have created a symlink:

```sh
ln -s $HOME/src/ambiata/box/_box.zsh $HOME/.config/zsh/completions/_box
```

and I have the following in my `.zshrc`:

```zsh
fpath=($HOME/.config/zsh/completions $fpath)
```

### fzf

[fzf](https://github.com/junegunn/fzf) is a general-purpose command-line
fuzzy finder. Below are some shell scripts which allow you to use it to
quickly select the environment and the instance.

To install `fzf` on OS/X, you can use `brew`:
```
$ brew install fzf
```

#### Z Shell

To integrate `fzf` with `box` on `zsh`, add the following to your `.zshrc`:

```zsh
fbox() {
  BOX_ENVIRONMENT=$(box --bash-completion-index 2 --bash-completion-word box --bash-completion-word -e | fzf)
  if [ ! -z "$BOX_ENVIRONMENT" ]; then
    BOX_INSTANCE=$(box -e $BOX_ENVIRONMENT ls | tail -n +2 | fzf | awk '{print $1 ":" $2 "::" $6}')
    if [ ! -z "$BOX_INSTANCE" ]; then
      print -z "box -e $BOX_ENVIRONMENT ssh $BOX_INSTANCE"
    fi
  fi
}
```

You can then use `fbox` to easily construct a `box ssh` command for
connecting to a box.

#### Bash

To integrate `fzf` with `box` on `bash`, add the following to your `.bashrc` or `.bash_profile`:

```bash
fbox() {
  BOX_ENVIRONMENT=$(box --bash-completion-index 2 --bash-completion-word box --bash-completion-word -e | fzf)
  if [ ! -z "$BOX_ENVIRONMENT" ]; then
    BOX_INSTANCE=$(box -e $BOX_ENVIRONMENT ls | tail -n +2 | fzf | awk '{print $1 ":" $2 "::" $6}')
    if [ ! -z "$BOX_INSTANCE" ]; then
      # add command to history so it can recalled quickly
      eval "history -s \"box -e $BOX_ENVIRONMENT ssh $BOX_INSTANCE\""
      # execute command
      eval "box -e $BOX_ENVIRONMENT ssh $BOX_INSTANCE"
    fi
  fi
}
```

This works slightly differently to the `zsh` integration because `bash`
lacks the equivalent of `print -z`. Instead we manually add the command
to the history, then execute the command, without giving the user
a chance to edit it.
