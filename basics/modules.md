# Creating a Haskell Module

> [!TIP]
> You can create a module using Powershell / Terminal\
> It is recommended you follow this step by step, to avoid creating unnecessary errors and spoiled files.

The following is a step by step to create the same module as I have provided in the folder.

1. Create and Navigate to your project directory
    - Replace `your-project-name` with your chosen directory name
    - ```
      > mkdir your-project-name
      > cd your-project-name
      ```
2. Save your Haskell code
    - Save as a `main.hs` file in the directory you created
3. Compile your `main.hs`
    - ```
      > ghc --make main.hs
      ```
4. Run the compiled executable
    - On Windows
    - ```
      > .\main.exe
      ```
    - On Unix (Mac/Linux)
    - ```
      > ./main
      ```
