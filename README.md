
		 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━	
				README


			Michaël Chirmeni Boujike
		 ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

# GP-RRC
Gradual Patterns for Recommendation in Repeat and Seasonal Consumption context

==========================================================================
			Files
==========================================================================
		1.	AprioriTID.jar	
		2.	GP-RRC.R
		3.  Param.csv
		4.	Datasets.csv


==========================================================================
			Files description
==========================================================================
		1.	"AprioriTID.jar" : modified java library of the AprioriTID algorithm;	
		2.	"GP-RRC.R" : generate emergents gradual pattern algorithm taking into account the gradual threshold;
		3.  "Param.csv" : contains neessary parameters to run program
			Example : <<minSupp>> <<minimum_size_of_gradual_pattern>> <<output_file>> <<K1>> <<K2>> <<column_separator_in_the_dataset>> <<windows>>
			<<num_date>> <<num_product>> <<num_quantite>> <<num_user>> <<top-N>> <<isClust>> <<number_clust>> <<minDiscSeason>> <<OneToOneOrOneToMany>>
			a. <<minSupp>> : allow to determine if gradual pattern is frequency;
			b. <<minimum_size_of_gradual_pattern>> : size minimum of gradual pattern;
			c. <<output_file>> : output file to will contains gradual patterns extract;
			d. <<K1>> : usefull when we take into account gradual threshold;
			e. <<K2>> : usefull when we take into account gradual threshold;
			f. <<column_separator_in_the_dataset>> : separator column in dataset file; 
			g. <<windows>> : determine the size of season;
			h. <<num_date>> : position of date in dataset file;
			i. <<num_product>> : position of product in dataset file;
			j. <<num_quantite>> : position of quantity in dataset file;
			k. <<num_user>> : position of user in dataset file;
			l. <<top-N>> : Top-N Recommendation;
			m. <<isClust>> : determine if the links between user are take into account;
			n. <<number_clust>> : number of cluster to consider;
			o. <<minDiscSeason>> : allow to determine if a gradual pattern is seasonal;
			p. <<OneToOneOrOneToMany>> : determine when scenario to use : 0 ==> to One-To-Many and 1 ==> to One-To-One
		4.	"Datasets.csv" : dataset; 

==========================================================================
		Run (WINDOWS)
==========================================================================	
		1.	Install R (latest version 3.6.x);
		2.	Put the bin of R in the environment variable (ex : C:\Program Files\R\R-3.6.0\bin);
		3.	After installing R, you must install the Java library for R:
			-	install.packages("rJava")
			-	if the Java JDK is not installed :
				*	download and install  jdk (latest version) : https://www3.ntu.edu.sg/home/ehchua/programming/howto/JDK_Howto.html
				*	create environment variable JAVA_HOME and put path until SDK (ex: JAVA_HOME ="C:\Program Files\Java\jdk1.8.0_171")
		4.	Go to the command prompt and go to the directory containing the source code then type :
			- "PATH\Rscript" <<File_R>> 
		   Example :
				- 	Rscript GP-RRC.R
==========================================================================
		Run (LINUX)
==========================================================================	
		1.	Install R (latest version 3.6.x);
		2.	After installing R, you must install the Java library for R:
			-	install.packages("rJava")
			-	if the Java JDK is not installed :
				*	download and install  jdk (latest version) : https://www3.ntu.edu.sg/home/ehchua/programming/howto/JDK_Howto.html
				*	create environment variable AVA_HOME and put path until SDK (ex: JAVA_HOME ="C:\Program Files\Java\jdk1.8.0_171")
		3.	Go to the command prompt and go to the directory containing the source code then type :
			- "PATH\Rscript" <<File_R>>
		
    Example :
				- 	Rscript GP-RRC.R  
				
				
				
