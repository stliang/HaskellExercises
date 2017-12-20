### Problem

Acme Avocado is a premier provider of high-quality avocados. But as we all
know, the avocado market is highly competitive, especially among discerning
millenials. Acme Avocado would like to understand which of their 8 customer
segments contains the most millenials with a high propensity to churn (i.e.
seek their avocados elsewhere).

Acme Avocado has prepared a customer data set in `tsv` format
(acme_customer_attributes.tsv):

```tsv
CustomerId  FirstName   LastName        Age   LTVEstimate ChurnScore  SegmentId
"59028934"  "Melanie"   "Gonzalez-Meza" "37"  "2798"      "0.273"     "8"
```

NOTE: The actual data will not include the header row. It is only shown here for
the purpose of communicating the problem.

Your task is to use the provided library to create a MapReduce job that will:

1. Find all millenials in the data set (age range between 18 and 32)
2. Filter out those with a churn score < 0.95
3. Group them by segment id
4. Generate an output `tsv` that displays the resulting count for each segment:

```tsv
SegmentId   Count
1           32
```

Start with the stub code provided in the directories `mapper` and `reducer`.

### Running the example solution:

#### With Haskell

```
$ cat acme_customer_attributes.tsv | stack exec mapper | sort | stack exec reducer | sort
```

#### With Scala

```
$ cat acme_customer_attributes.tsv | sbt mapper/run | sort | sbt reducer/run | sort
```

#### Expected Output

```
1	8
2	3
3	6
4	3
5	8
6	1
7	9
8	4
```
