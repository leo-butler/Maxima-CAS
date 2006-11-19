$main_info = $ARGV[0];
$index_node_name = $ARGV[1];

$unit_separator = "";

# ------------------------------------------------------------------
# PART 1. BUILD INDEX FOR @DEFFN AND @DEFVR ITEMS
# ------------------------------------------------------------------

# (1.1)  Slurp indexing data from the *.info and *.info-* files.

# (1.1a) Read the "Indirect:" table, which gives the nominal byte offset
#        at the start of each *.info-* file.
#        For *.info-2 and after, the nominal offset must be adjusted
#        by the number of bytes of header data before the first
#        unit separator character.

open (FH, $main_info);
read (FH, $stuff, -s FH);

while ($stuff =~ m/^($main_info-\d+): (\d+)/cgsm) {
    $filename = $1;
    $offset = $2;

    open FH2, $filename;
    read FH2, $stuff2, -s FH2;
    if ($filename =~ m/-1$/) {
        $nominal_initial_offset{0} = $filename;
        # Node offsets reported for first file already take into account header size.
        $header_size{$filename} = 0;
    }
    else {
        $nominal_initial_offset{int($offset)} = $filename;
        # Node offsets reported for second and later files do not account for header size. Sigh.
        if ($stuff2 =~ m/.*?$unit_separator/cgsm) {
            $header_size{$filename} = (pos $stuff2) - 1;
        }
    }
    close $FH2;
}

# (1.1b) Read the "Node:" table, which gives the nominal byte offset
#        for each info node.

while ($stuff =~ m/^Node: ([^]+)(\d+)$/cgsm) {
    $node_name = $1;
    $offset = $2;
    $node_offset{$node_name} = int($offset);
}

close FH;

# (1.1c) Read the info index, which gives the node name and number of lines offset
#        for each indexed item. 

$index_node_offset = $node_offset{$index_node_name};
@a = map_offset_to_filename ($index_node_offset, %nominal_initial_offset);
$index_filename = $a[1];

open (FH, $index_filename);
read (FH, $stuff, -s FH);

if ($stuff =~ m/^File:.*?Node: $index_node_name.*^\* Menu:/icgsm) {
    while ($stuff =~ m/\G.*?^\* (\S+|[^:]+):\s+(.*?)\.\s+\(line\s+(\d+)\)/cgsm) {
        $topic_name = $1;
        $node_name = $2;
        $lines_offset = $3;
        $topic_locator{$topic_name} = [($node_name, $lines_offset)];
    }
}

close FH;

# (1.2)  Translate node name and number of lines offset into file name and byte offset
#        for each indexed item.
#        Also find the length of each item.

foreach $key (sort keys %topic_locator) {
    ($node_name, $lines_offset) = @{$topic_locator{$key}};
    ($initial_offset, $filename) = map_offset_to_filename ($node_offset{$node_name}, %nominal_initial_offset);
    $byte_offset = $header_size{$filename} + $node_offset{$node_name} - $initial_offset;
    $byte_offset = seek_lines($filename, $byte_offset, $lines_offset - 1);

    open FH, $filename;
    seek FH, $byte_offset, 0;
    read FH, $stuff, -s FH;
    if ($stuff =~ m/(?:\n\n(?= -- )|\n(?=[0-9])|(?=$unit_separator))/cgsm) {
        $text_length = pos $stuff;
    }
    else {
        # Eat everything up til end of file.
        $stuff =~ m/.*/cgsm;
        $text_length = pos $stuff;
    }
    close FH;

    $topic_locator{$key} = [($node_name, $filename, $byte_offset, $text_length)];
}

# (1.3)  Generate Lisp code. The functions in info.lisp expect this stuff.

print "(in-package :cl-info)\n";
print "(defun cause-describe-index-to-load () nil)\n";

#        Pairs of the form (<index topic> . (<filename> <byte offset> <length> <node name>))

print "(defvar *info-deffn-defvr-pairs* '(\n";

foreach $key (sort keys %topic_locator) {
    print "(\"$key\" . (\"$topic_locator{$key}[1]\" $topic_locator{$key}[2] $topic_locator{$key}[3] \"$topic_locator{$key}[0]\"))\n";
}

print "))\n";

# ------------------------------------------------------------------
# PART 2. BUILD INDEX FOR @NODE ITEMS
# ------------------------------------------------------------------

# (2.1)  Search for 'mmm.nnn' at the start of a line,
#        and take each one of those to be the start of a node.
#
#        We could use the node table ($node_offset here), but we don't.
#        (a) The node table indexes nodes which contain only menus.
#            We don't want those because they have no useful text.
#        (b) The byte offset stated in the node table tells the location
#            of the "File: ..." header. We would have to cut off that stuff.
#        (c) The following regex-only code works OK as it stands.

foreach $filename (sort values %nominal_initial_offset) {

    open (FH, $filename);
    read (FH, $stuff, -s FH);

    while ($stuff =~ m/\G.*?((^\d+\.\d+) (.*?)\n)/cgsm) {

        $begin_node_offset = pos($stuff) - length($1);
        $node_title = $3;

        # ?? $pos1 = pos($stuff);

        # Node text ends at a unit separator character,
        # or at the end of the file.

        if ($stuff =~ m/\G.*?($unit_separator)/cgsm) {
            $end_node_offset = pos($stuff) - length($1);
        }
        else {
            $stuff =~ m/\G.*/csgm;
            $end_node_offset = pos($stuff);
        }

        # ?? pos($stuff) = $pos1;

        $node_locator{$node_title} = [($filename, $begin_node_offset, $end_node_offset - $begin_node_offset)];
    }

    close FH;
}

# (2.2)  Generate Lisp code.
#
#        Pairs of the form (<node name> . (<filename> <byte offset> <length>))

print "(defvar *info-section-pairs* '(\n";

foreach $node_title (sort keys %node_locator) {
    ($filename, $begin_node_offset, $length) = @{$node_locator{$node_title}};
    print "(\"$node_title\" . (\"$filename\" $begin_node_offset ", $length, "))\n";
}

print "))\n";

#        Construct hashtables from the lists given above.

print "(load-info-hashtables)\n";

# ------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------

sub map_offset_to_filename {
    my ($offset, %offset_filename_hash) = @_;
    $floor_offset = 0;
    foreach $key (sort {$a <=> $b} keys %offset_filename_hash) {
        if ($offset >= $key) {
            $floor_offset = $key;
        }
    }

    return ($floor_offset, $offset_filename_hash{$floor_offset});
}

sub seek_lines {
    my ($filename, $byte_offset, $lines_offset) = @_;
    open FH, $filename;
    seek FH, $byte_offset, 0;

    # MAKEINFO BUG: LINE OFFSET IS LINE NUMBER OF LAST LINE IN FUNCTION DEFINITION
    # (BUT WE NEED THE FIRST LINE OF THE FUNCTION DEFINITION)
    # BUG IS PRESENT IN MAKEINFO 4.8; FOLLOWING CAN GO AWAY WHEN BUG IS FIXED
    
    for (1 .. $lines_offset + 1) {
        my $x_maybe = tell FH;
        my $line = <FH>;
        if ($line =~ /^ -- \S/) {
            $x = $x_maybe;
        }
    }

    # END OF MAKEINFO BUG WORKAROUND
    # WHEN WORKAROUND IS NO LONGER NEEDED, ENABLE THE FOLLOWING LINES:

    # <FH> for 1 .. $lines_offset;
    # $x = tell FH;

    close FH;
    return $x;
}
