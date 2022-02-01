*Danielle LaVine*

### Overall Grade: 107/120

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts per day for late submission.

    Yes.

-   Is the final report in a human readable format html? 

    Yes.

-   Is the report prepared as a dynamic document (R markdown) for better reproducibility?

    Yes.

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take some points off if the solutions are too succint to grasp, or there are too many typos/grammar. 

    Yes.

### Completeness, correctness and efficiency of solution: 62/70

- Q1 (10/10)

- Q2 (20/20)

- Q3 (8/20)

    3.4 - We want to count the number of uncompressed lines. use `zcat` first. `-5.`
    
    3.5 - Remove 1 from the number of unique patients because you are including the header. Or use `-tail n+2`. `-5`.
    
    3.6 - Show the values of eachc category by using `uniq -c`. `-2.`

- Q4 (10/10)

	It's fine to just count the lines containing each name. If a student figures out a way to count the words (one line may contain the same name multiple times), give bonus points.

- Q5 (10/10)
	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out not clustered the day before deadline. 
          
-   Is the hw1 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put a lot auxiliary files into version control. 

-   If those gz data files or `pg42671` are in Git, take 5 points off.

### Reproducibility: 9/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `knit` button will produce the final `html` on teaching server? 

    We want lines 180-199 and 234-236 to be `eval = T` so we produce our output. When we `knit`. When you change it to TRUE, the correct answers are there. `-1.` 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 16/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 3.](https://google.github.io/styleguide/Rguide.xml#linelength) The maximum line length is 80 characters. 

    Lines 128, 143, 154 were all over 80 characters for 3.5 and 3.6. `-4`.

-   [Rule 4.](https://google.github.io/styleguide/Rguide.xml#indentation) When indenting your code, use two spaces.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place spaces around all binary operators (=, +, -, &lt;-, etc.). 
	
-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place a space before a comma, but always place one after a comma. 

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Place a space before left parenthesis, except in a function call.

-   [Rule 5.](https://google.github.io/styleguide/Rguide.xml#spacing) Do not place spaces around code in parentheses or square brackets.
