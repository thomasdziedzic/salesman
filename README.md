salesman
========

sales(force) man(ager)

## Installation

1. Dependencies: ghc, cabal-install, git, java.

2. Clone repository.
    ```
    git clone https://github.com/gostrc/salesman.git
    ```

3. Download the tooling api tool
    - You can find releases at https://github.com/neowit/tooling-force.com/releases
    - Download the released jar to ```~/.salesman/```
    - Create a symlink from ```~/.salesman/tooling-force.com.jar``` to the location you downloaded the jar file e.g. ```cd ~/.salesman && ln -s tooling-force.com-0.1.4.3.jar tooling-force.com.jar```

4. Install
    ```
    cabal install
    ```

Tip: add ```$HOME/.cabal/bin``` to your ```PATH``` so that you do not have to use absolute/relative paths to run salesman.

## Usage

### Format of properties file

The properties file is the same standard format salesforce uses for its ant migration tool:

```
sf.username=USERNAME
sf.password=PASSWORD
sf.serverurl=https://test.salesforce.com
```

Note: use ```sf.serverurl=https://login.salesforce.com``` for production and developer instances.

Note: append the security token to the password if you need it.

### Installing packages

```
salesman --properties /path/to/properties install PackageA
```

### Removing packages

```
salesman --properties /path/to/properties remove PackageA
```

### Listing installed packages

```
salesman --properties /path/to/properties list
```
