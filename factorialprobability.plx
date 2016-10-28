#!/usr/bin/perl
use strict;
use warnings;

# Perl is slow but for a fairly small number of iterations, this is a very
# easy and illustrative example for programming probability theory.
# This example takes ten coin flips and works up the likelihood of 
# one head, then two heads, etc. It also provides tab-separated output
# for easy importing into other applications (R, Stata, Excel, etc.)
# The R code to ingest and nicely display these data is contained in headplot.R,
# which is reproduced below for completeness.

my $coins=10;
# &factorial($coins);

my $heads=0;
my $trials=10;
my $probability=0.5;
my $countposs=0;

my $sumcounts=&countProbability($trials,$heads);
print "The sum of all possible outcomes is $sumcounts\n\n";

for $heads (0..10) 
{
	my $probtest=&binomialProb($trials,$heads,$probability);
	print "Probability of $heads heads in $trials trials at P=$probability per coin toss is $probtest\n";
	$countposs=&nChooseR($trials,$heads);
	print "\t( ",$countposs," ",&chances($countposs) ," in $sumcounts )\n";
}

print "\n";

&printData($trials,$heads,$probability);

######################## SUBROUTINES BELOW THIS LINE ########################

sub factorial {
	my ($input)=shift;
	if ($input==0) { return 1;}
	if ($input<0) { die "Hey dork, you can't do that.\n\n"; }
	my $fac=$input;
	my $c = $input;
	while ($c>1)
	{
		$fac*=($c-1);
		$c--;
	}
	# print "factorial of $input is $fac\n\n";
	return $fac;
}

sub binomialProb {
	my ($n,$x,$p)=@_;
	my $prob= (&nChooseR($n,$x)) * $p**$x * (1-$p)**($n-$x) ;
return $prob;	
}

sub nChooseR {
	my ($n,$x)=@_;
	my $nchooser = ( &factorial($n) ) / (&factorial($x) * &factorial($n-$x));
	return $nchooser;	
}

sub countProbability {
	my ($n,$x) = @_;
	my $sum=0;
	
	for my $i (0..$n) 
	{
		print "$i heads: ",&nChooseR($n,$i) ," + $sum = " ;
		$sum+= (&nChooseR($n,$i));
		print "$sum \n";
	}
	return $sum;
}

sub chances {
	my $count=shift;
	if ($count==1) { return "chance" }
	return "chances"; 	
}

sub printData {
	my ($n,$x,$p)=@_;

	open (OUT, ">heads.tab" ) or die "Aiegh! Can't open file for writing!\n";
	print OUT "HEADCOUNT\tPOSSIBLE\tPROB\n";
	for my $heads (0..$n)
	{
		print OUT "$heads\t";
		print OUT &nChooseR($n,$heads),"\t";
		print OUT &binomialProb($n,$heads,$p),"\n";
	}
	close OUT;
	return 1;
}

__DATA__
# Laying the foundation for probability distribution theory
# This plot of coin flips makes a nice illustration of 
# a probability distribution function

# setwd("")

a<-(read.delim("heads.tab", header=TRUE, sep="\t"))

png(filename="headprobabilities.png", width=5, height=5, units="in", pointsize=10, bg="white", res=300, type="quartz")

barplot(a$POSSIBLE, xlab="Count of Heads", ylab="Number of Possible Outcomes With That Many Heads", names.arg=a$HEADCOUNT, col="red",bg="black", axes=TRUE, main="Possible Outcomes of Heads in 10 Coin Flips")
axis(4, at=axTicks(2), label=prettyNum(axTicks(2) /1024,width=4,digits=3 ))
text (13,100,labels="Probability", pos=4, cex=1.5, srt=90, col='blue')

dev.off()

# End of File.
