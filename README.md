# Statistics-Fundamentals-in-R


We call properties with varying values **variables**. 
Variables in statistics can describe either **quantities** or **qualities**.


Generally, a variable that describes how much there is of something describes a quantity, and, for this reason, it's called a **quantitative variable**.

— a property that is not quantitative. Variables that describe qualities are called **qualitative variables** or **categorical variables.**
 Qualitative variables describe what or how something is.

For any variable measured on a **nominal scale:**

- We can tell whether two individuals are different or not (with respect to that variable).
- We can't say anything about the direction and the size of the difference.
- We know that it can only describe qualities.

Generally, for any variable measured on an **ordinal scale**, we can tell whether individuals are different or not. 
We can also tell the direction of the difference, but we still can't determine the size of the difference.

Variables measured on an ordinal scale can only be quantitative.
 Quantitative variables, however, can be measured on other scales. 

A variable measured on a scale that preserves the order between values and has well-defined intervals using real numbers is an example of a variable measured either on an interval scale, or on a ratio scale.

In practice, variables measured on interval or ratio scales are very common, if not the most common. Examples include:

- Height measured with a numerical unit of measurement (inches or centimeters).
- Weight measured with a numerical unit of measurement (multiples and submultiples of grams).
- Time measured with a numerical unit of measurement (multiples and submultiple of seconds).
- The price of various products measured with a numerical unit of measurement (dollars, pounds, etc.).


On a ratio scale, we can quantify the difference in two ways. One way is to measure a distance between any two points by simply subtracting one from another.
 The other way is to measure the difference in terms of ratios.

On an interval scale, however, 
we can measure meaningfully the difference between any 
two points only by finding the distance between them 
(by subtracting one point from another).

Generally, if there's no possible intermediate value between any two adjacent 
values of a variable, we call that variable **discrete**.


Common examples of discrete variables include counts of people in a class,
 a room, an office, a country, a house etc. For instance, if we counted the number of
 people living in each house of a given street, the results of our counting could only be integers. 
For any given house, we could count 1, 3, 7, 0 people, but we could not count 2.3 people, or 4.1378921521 people.


Generally, if there's an infinity of values between any two values of a variable, 
we call that variable **continuous**


Generally, every value of a continuous variable is an interval, no matter how precise the value is. The boundaries of an interval are sometimes called real limits.
 The lower boundary of the interval is called **lower real limit**, and the upper boundary is called **upper real limit.**


One way to simplify this dataset is to select a variable,
 count how many times each unique value occurs, and represent
 the **frequencies** (the number of times a unique value occurs) in a table. 



Because proportions and percentages are relative to the total number
 of instances in some set of data, they are called relative frequencies. 
In contrast, the frequencies we've been working with so far are called absolute frequencies because they 
are absolute counts and don't relate to the total number of instances.


**Generating a frequency distribution table for a column (col):**

`df %>%
  group_by(col) %>%
  summarize(Freq = n())`

**Sorting the values of frequency distribution table in ascending order (default):**

`df %>%
  group_by(col) %>%
  summarize(Freq = n())`

**Sorting in ascending order (being explicit):**

`df %>%
  group_by(col) %>%
  summarize(Freq = n()) %>% 
  arrange(col)`

**Sorting in descending order:**

`df %>%
  group_by(col) %>%
  summarize(Freq = n()) %>% 
  arrange(desc(col))`

**Finding proportions and percentages in a frequency distribution table:**

`df %>%
  group_by(col) %>%
  summarize(Freq = n()) %>% 
  mutate(Prop = Freq / nrow(df)) %>%
  mutate(Percentage = Freq / nrow(df) * 100) %>% 
  arrange(desc(Freq))`

**Finding the percentile rank of a value (score) in a column:**

`mean(df$col <= value) * 100`

**Finding percentiles - only the quartiles:**

`quantile(df$col)`

**Finding any percentile we designate:**

`quantile(df$col, 
            probs = c(0, 0.1, 0.25, 0.33, 0.5, 0.66, 0.75, 0.9, 1))`

**Generate percentiles for each value in a dataframe:**

`df %>% 
  mutate(cume_dist_col = cume_dist(col))`

**Generating a grouped frequency table:**

`df <- df %>% 
  mutate(categories = cut(col, breaks = 5, dig.lab = 4))
df %>% 
  group_by(categories) %>% 
  summarize(Freq = n())`

**Generating a grouped frequency table with custom class intervals:**

`df <- df %>% 
  mutate(categories = 
           cut(col, 
               breaks = c(0, 150, 300, 450, 
               600, 750, 900, 1050), 
               dig.lab = 4))`

**Showing frequency and percentage**

`df %>% 
  group_by(categories) %>% 
  summarize(Freq = n()) %>% 
  mutate(Percentage = Freq / nrow(df) * 100)`
