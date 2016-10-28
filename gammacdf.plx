#!/usr/bin/perl -w
use strict;
use warnings;

my $theta=2;
my $k=3;
my @temp=();
my ($answer, $subtractor);
my ($c1,$c2,$c3);

my $verbose=1;

foreach my $x (0..30) 
{
	@temp=();
	foreach my $i (0..$k-1)
	{
		$c3=(exp (-$x/$theta));
		$c2=&factorial($i);
		$c1=($x/$theta)**$i;
		$temp[$i]=($c1/$c2) * $c3;
        if ($verbose) {
		    print "C1 is $c1; C2 is $c2; C3 is $c3; TEMP is ";
		    print "$c1\n---- * $c3 = ".($c1/$c2) * $c3;
		    print "\n$c2\n\n";
    		print "$temp[$i]\n";
        }

	}
	print "for X=$x, the CDF is ";
	$subtractor=0; $answer=0;
	foreach (@temp) 
	{

		print "adding $_ to $subtractor...\n" if $verbose  ;
		$subtractor	+= $_;
		
	}
	$answer=1-$subtractor;
	print "$answer\n";
}

sub factorial
{
	my $input=shift;
	my $s=1;
	my $r=1;
	while ($s <= $input)
	{
		$r *= $s;
		$s++;
	}
	if ($input == 0) # Remember: 0! is equal to 1
	{
		$r=1;
	}
return $r;
}


