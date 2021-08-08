# Mathematics

Here you can find the explanation of mathematics used in the smart-contract,
including Python and SageMath code used for generating constants.

## The notebook

Start from `Maths.ipynb` – it is a Jupyter notebook with a SageMath kernel.
It explains the algorithms and approximations used in the smart-contract,
estimates the errors, and generates the constants.

To work with the notebook, you will need [SageMath][sage].

### With Nix

If you use [Nix], you don’t need to install anything.
Just `cd` into this directory and run
(if you have unsble Nix with flakes support):

```text
nix run nixpkgs#sage -- -n jupyter
```

or (if using stable Nix without flakes support):

```text
nix run nixpkgs.sage -c sage -n jupyter
```

### Without Nix

Follow the official [installation guide][sage:install] to instal SageMath
and then run:

```text
sage -n jupyter
```


[sage]: https://www.sagemath.org/
[sage:install]: https://doc.sagemath.org/html/en/installation/
