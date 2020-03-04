#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

"copy from example-files/charfreq.st"
Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [ ^ theArray size.  ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

"copy from example-files/tree.st, add inorder and postorder"
Object subclass: Leaf [
   |char count isLeaf|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [^ isLeaf]
   
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.      
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isLeaf:=true.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   
   postorder: visitor [
      visitor  value:char value: isLeaf.
   ]
]

"copy from example-files/tree.st, add inorder and postorder"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isLeaf:=false.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor[      
      left postorder: visitor .
      right postorder: visitor .  
      visitor value: char value:isLeaf.
   ]
]

"copy from example-files/outbits.st"
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: stream [
      |result|
      result := super new.
      result init: stream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: stream [
      myStream := stream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"copy from example-files/bitstream.st"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: stream [
      |result|
      result := super new.
      result init: stream.
      ^result
   ]
   init: stream [
      myStream := stream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"check arguments size"
argc := Smalltalk arguments size

((argc < 2) | (argc > 3))
ifTrue: [
    stderr << 'Error: invalid arguments size' << nl.
    stderr << 'Usage: hzip.st -dtcu inputfile [outputfile]' << nl.
    ObjectMemory quit
].

"open input file and output file"
infilename := Smalltalk arguments at: 2.
infile := FileStream open: infilename
                     mode: FileStream read.

outfile := stdout.
(argc = 3) ifTrue: [
    outfilename := Smalltalk arguments at: 3.
    outfile := FileStream open: outfilename
                          mode: FileStream write.
].

"main logic"
Smalltalk arguments: '-d -c -t -u'
do: [:opt :arg |
    queue := SortedCollection new.

    (opt = $u) ifTrue: [
        iseof := false.
        out := OutBits new: outfile.  

        done := false.
        root := ''.
        node := ''.   

        stack := OrderedCollection new.
        stream := BitStream new: infile.
   
        [(stream atEnd not) & (iseof not)] whileTrue: [
            bit := stream nextBit.
            byte := 0.
            done ifFalse: [
                bit = 0 ifTrue: [
                    8 to: 1 by: -1 do: [:i|
                        byte := byte bitAt: i put: (stream nextBit).
                    ].
                    byte = 0 ifTrue: [
                        nbit := stream nextBit.
                        nbit = 1 ifTrue: [
                            byte := byte bitAt: 9 put: 1.
                        ].
                    ].
                    stack add: (Leaf new: byte count: '').
                ].
                bit = 1 ifTrue: [
                    stack size = 1 ifTrue: [
                        done := true.
                        root := stack removeLast.
                        node := root.
                    ]      
                    ifFalse: [
                        rb := stack removeLast.
                        lb := stack removeLast.
                        new_tree := (Tree new: -1
                                          count: ''
                                          left: lb right: rb).
                        stack add: new_tree.
                    ]. 
                ].
            ]
          
            ifTrue: [
                node isLeaf ifTrue: [
                    ((node char) = 256) ifTrue: [
                        iseof := true.
                    ] 
                    ifFalse: [
                        stdout nextPut: (Character value: (node char)).
                        node := root.
                    ].
                ]
                ifFalse: [
                    bit = 0 ifTrue: [
                        node := node left.
                    ] 
                    ifFalse: [
                        node := node right.
                    ].
                    node isLeaf ifTrue: [
                        iseof not ifTrue: [
                          ((node char) = 256) ifTrue: [
                              iseof := true.
                          ] 
                          ifFalse: [
                              8 to: 1 by: -1 do: [:index |
                                out writeBit:((node char) bitAt:index).
                              ].

                              node := root.
                          ].
                        ].
                    ].
                ].
            ].
        ].
    ]

    ifFalse: [
        "start creating freq table"
        freq_table := ZeroArray new: 260.
        freq_table at: 256 put: 1.

        [infile atEnd not] whileTrue: [
            aChar := infile next asInteger.
            freq_table incr: aChar.
        ].

        freq_table keysAndValuesDo: [:key :count |
           (count notNil and: [count > 0]) ifTrue: [
              queue add: (Leaf new: key count: count).
            ]
        ].

        "start creating priority queue"
        [queue size > 1 ] whileTrue: [
            |lb rb|
            lb := queue removeFirst.
            rb := queue removeFirst.
            new_tree := (Tree new: (lb char)
                              count: (lb count) + (rb count)
                              left: lb right: rb).
            queue add: new_tree.
        ].

        "start creating encode table"
        encode_table := ZeroArray new: 260.
        tree := queue removeFirst.
        tree inorder: [:char :string |
            encode_table at: char put: string.
        ] prefix: ''.
    ].

    (opt = $c) ifTrue: [
        out := OutBits new: outfile.    
        tree postorder: [:char :isLeaf |
            isLeaf ifFalse: [
                out writeBit: ('1' asInteger) - 48.
            ]
            ifTrue: [
                bin := ''.
                char = 256 ifTrue: [
                  bin := '000000001'
                ]
                ifFalse: [
                    char = 0 ifTrue: [
                        bin := '000000000'
                    ]
                    ifFalse: [
                        8 to: 1 by: -1 do: [:index |
                           bin := bin,
                            ((char bitAt:index) printString).
                        ]
                    ].
                ].

                out writeBit: ('0' asInteger) - 48.
                1 to: (bin size) by: 1 do: [:index |
                    bit:= ((bin at: index) asInteger) - 48.
                    out writeBit: bit.
                ].
            ].
        ].
      
        out writeBit: 1.

        infile reset.

        [infile atEnd not] whileTrue: [
            aChar := infile next asInteger.
            code:= (encode_table at: aChar). 
            1 to: (code size) by: 1 do: [:index |
                bit:= ((code at: index) asInteger) - 48.
                out writeBit: bit.
            ].
        ].

        eof := (encode_table at: 256).
        1 to: (eof size) by: 1 do: [:index |
            bit:= ((eof at: index) asInteger) - 48.
            out writeBit: bit.
        ].

        out flushByte.    
    ].

    (opt = $t) ifTrue: [
        encode_table keysAndValuesDo: [:key :value |
            (value notNil and: [value > 0]) ifTrue: [
                |index char count string pad|
                key = 10 ifTrue: [
                    char := 'x0A'.
                ] 
                ifFalse: [
                    key = 32 ifTrue: [
                        char := 'x20'.
                    ]
                    ifFalse: [
                       key = 256 ifTrue: [
                           char := 'EOF'.
                       ] 
                        ifFalse: [
                           char := ' ', 
                               key asCharacter asString, ' '.
                        ].
                    ]
                ].

                stdout << char << ' '.
                count := (freq_table at: key) printString.
                pad := 7 - count size.
                [pad > 0] whileTrue: [
                    count := ' ', count.
                    pad := pad - 1.
                ].
                stdout << count << '  '.
                string := value.
                stdout << string << nl.
            ].
        ].
    ].
].
