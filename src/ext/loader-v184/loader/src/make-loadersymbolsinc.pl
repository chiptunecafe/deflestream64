#!/usr/bin/env perl

=head1 NAME

make-loadersymbolsinc.pl

=head1 DESCRIPTION

This script extracts relevant symbols from a ld65-generated map file.
The resulting include file can be included in third-party assembly source code.

=head1 SYNOPSIS

  make-loadersymbolsinc.pl loader-c64.map > loadersymbols-c64.inc

=cut

use strict;
use warnings;

use English qw( -no_match_vars ); # OS_ERROR etc.


main();

sub main
{
    my @input = read_file($ARGV[0]);
    my ($diskio_segments, $symbols) = extract_all_symbols(@input);
    print_symbols($diskio_segments, filter_symbols($diskio_segments, $symbols));

    return;
}

sub read_file
{
    my ($filename) = @ARG;

    open(my $fh, '<', $filename)
        or die "cannot read file '$filename': $OS_ERROR";

    my @input = <$fh>;
    close $fh;

    return @input;
}

sub extract_all_symbols
{
    my @input = @ARG;

    my $current_list;
    my %diskio_segments;
    my %symbols;
    foreach my $i (@input) {

        if ($i =~ qr{list.*?:}) {
            $current_list = $i;

        } elsif ($current_list =~ qr{Segment}) {
            # only check against the DISKIO segments
            if ($i =~ qr{(DISKIO\w*)\s+(\w+)\s+(\w+)}) {
                my $label = lc $1;
                $diskio_segments{$label . '_start'} = hex '0x' . $2;
                $diskio_segments{$label . '_end'}   = hex '0x' . $3;
            }

        } elsif ($current_list =~ qr{Exports}) {
            # the exported symbols will be filtered by the DISKIO segments
            if ($i =~ qr{(\w+)\s+(\w+)\s+\w+\s+(\w+)\s+(\w+)}) {
                # double-entry line
                $symbols{$1} = hex '0x' . $2;
                $symbols{$3} = hex '0x' . $4;
            } elsif ($i =~ qr{(\w+)\s+(\w+)\s+\w+}) {
                # single-entry line
                $symbols{$1} = hex '0x' . $2;
            }
        }
    }

    return \%diskio_segments, \%symbols;
}

sub filter_symbols
{
    my ($diskio_segments, $symbols) = @ARG;

    my %filtered;
    while (my ($name, $address) = each %$symbols) {
        # do not regard *fix symbols, all-caps or *1 (41/71/81) symbols
        if ($address && ($name !~ /fix|[A-Z]|1$/)) {
            # only symbols in the DISKIO segments are printed
            if ((($address >= $diskio_segments->{diskio_start})
              && ($address <= $diskio_segments->{diskio_end}))

             || (($address >= $diskio_segments->{diskio_install_start})
              && ($address <= $diskio_segments->{diskio_install_end}))

             || (($address >= $diskio_segments->{diskio_zp_start})
              && ($address <= $diskio_segments->{diskio_zp_end})))
            {
                $filtered{$name} = $address;
            }
        } elsif (($name =~ /config_/) || ($name =~ /status_/)) {
            $filtered{$name} = $address;
        }
    }

    return \%filtered;
}

sub print_symbols
{
    my ($diskio_segments, $symbols) = @ARG;

    my @symbols_sorted_by_address =
        sort { $symbols->{$a} <=> $symbols->{$b} } keys %{$symbols};

    chomp(my $datetime = `date -R`);
    print "; built on " . $datetime . " using\n";
    print "; make PLATFORM=" . $ENV{'_PLATFORM_'} . " prg INSTALL=" . $ENV{'INSTALL'} . " RESIDENT=" . $ENV{'RESIDENT'} . " ZP=" . $ENV{'ZP'} . "\n\n";

    print "; configuration\n";
    foreach my $name (sort (keys(%{$symbols}))) {
        if ($name =~ /config_/) {
            print_symbol($name, $symbols->{$name});
        }
    }

    print "\n; status codes\n";
    foreach my $name (@symbols_sorted_by_address) {
        if ($name =~ /status_/) {
            print_symbol($name, $symbols->{$name});
        }
    }

    print "\n; zeropage\n";
    print_symbol("loader_zp_first", $symbols->{"loader_zp_first"});
    print_segment_symbols($symbols, $diskio_segments->{diskio_zp_start}, $diskio_segments->{diskio_zp_end}, @symbols_sorted_by_address);

    print "\n; install\n";
    print_segment_symbols($symbols, $diskio_segments->{diskio_install_start}, $diskio_segments->{diskio_install_end}, @symbols_sorted_by_address);

    print "\n; resident\n";
    print_segment_symbols($symbols, $diskio_segments->{diskio_start}, $diskio_segments->{diskio_end}, @symbols_sorted_by_address);

    return;
}

sub print_segment_symbols
{
    my ($symbols, $start, $end, @symbols_sorted_by_address) = @ARG;

    foreach my $name (@symbols_sorted_by_address) {
        if ($name =~ /config_|status_|loader_zp_first/) {
            next;
        }

        my $address = $symbols->{$name};
        if ($address == 0) {
            next;
        }

        if (($address >= $start)
         && ($address <= $end)) {
            print_symbol($name, $symbols->{$name});

            $symbols->{$name} = 0;
        }
    }

    return;
}

sub print_symbol
{
    my ($name, $address) = @ARG;

    my $address_length = ($address < 256) ? '2' : '4';
    my $format = '%-31s = $%.' . $address_length . "x\n";
    printf $format, $name, $address;

    return;
}
