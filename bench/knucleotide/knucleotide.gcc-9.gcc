// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Jeremy Zerfas

// This controls the initial size used for the hash tables.
#define INITIAL_HASH_TABLE_SIZE 64
// This controls the maximum length for each set of nucleotide sequence
// frequencies and each nucleotide sequence count output by this program.
#define MAXIMUM_OUTPUT_LENGTH 4096

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct ht_ht ht_ht;
typedef struct ht_node ht_node;

// intptr_t should be the native integer type on most sane systems.
typedef intptr_t intnative_t;


// The hash table implementation provided by simple_hash3.h doesn't
// automatically grow hash tables (although the chained linked lists can grow
// infinitely long) and it also doesn't provide any function for growing a hash
// table so we create our own function for checking the hash table density and
// growing the hash table if necessary.
static void check_Hash_Table_Density_And_Grow_If_Necessary(
  ht_ht ** const hash_Table_To_Check){
   if(ht_count(*hash_Table_To_Check) > (*hash_Table_To_Check)->size){
      // Create a new grown_Hash_Table which is at least four times the size
      // of the current hash_Table_To_Check.
      ht_ht * grown_Hash_Table=ht_create(4 * (*hash_Table_To_Check)->size);

      // Copy all the ht_nodes from the current hash_Table_To_Check to the new
      // grown_Hash_Table.
      ht_node *HT_Node_Pointer=ht_first(*hash_Table_To_Check);
      for(intnative_t i=0; i<ht_count(*hash_Table_To_Check); i++){
         ht_find_new(grown_Hash_Table, HT_Node_Pointer->key)->val=
           HT_Node_Pointer->val;
         HT_Node_Pointer=ht_next(*hash_Table_To_Check);
      }

      // Destroy the old hash_Table_To_Check and update its pointer to point
      // to the new grown_Hash_Table.
      ht_destroy(*hash_Table_To_Check);
      *hash_Table_To_Check=grown_Hash_Table;
   }
}


// Function to use when sorting ht_nodes with qsort() later. ht_nodes with
// larger values will come first and in cases of identical values then ht_nodes
// with smaller keys will come first.
static int HT_Node_Compare(const void * const uncasted_Left_HT_Node,
  const void * const uncasted_Right_HT_Node){
   const ht_node * left_HT_Node=uncasted_Left_HT_Node,
     * right_HT_Node=uncasted_Right_HT_Node;

   // Sort based on ht_node values.
   if(left_HT_Node->val < right_HT_Node->val) return 1;
   if(left_HT_Node->val > right_HT_Node->val) return -1;

   // If we got here then both items have the same value so then sort based on
   // key.
   if(left_HT_Node->key > right_HT_Node->key)
      return 1;
   else
      return -1;
}


// Macro to convert a nucleotide character to a code. Note that upper and lower
// case ASCII letters only differ in the fifth bit from the right and we only
// need the three least significant bits to differentiate the letters 'A', 'C',
// 'G', and 'T'. Spaces in this array/string will never be used as long as
// characters other than 'A', 'C', 'G', and 'T' aren't used.
#define code_For_Nucleotide(nucleotide) (" \0 \1\3  \2"[nucleotide & 0x7])


// And one more macro to convert the codes back to nucleotide characters.
#define nucleotide_For_Code(code) ("ACGT"[code & 0x3])


// Generate frequences for all nucleotide sequences in sequences that are of
// length sequence_Length and then save it to output.
static void generate_Frequencies_For_Sequences(const char * const sequences,
  const intnative_t sequences_Length, intnative_t sequence_Length,
  char * const output){
   ht_ht * hash_Table=ht_create(INITIAL_HASH_TABLE_SIZE);

   // Add all the sequences of sequence_Length to hash_Table.
   uint64_t key=0;
   for(intnative_t i=0; i<sequences_Length; i++){
      const uint64_t mask=((uint64_t)1<<2*sequence_Length)-1;
      key=(key<<2 & mask) | sequences[i];
      if(i>=sequence_Length-1){
         ht_find_new(hash_Table, key)->val++;
         check_Hash_Table_Density_And_Grow_If_Necessary(&hash_Table);
      }
   }

   // Create an array of ht_nodes from hash_Table.
   intnative_t HT_Nodes_Array_Size=hash_Table->items;
   ht_node * HT_Nodes_Array=malloc(HT_Nodes_Array_Size*sizeof(ht_node));
   ht_node * HT_Node_Pointer=ht_first(hash_Table);
   for(intnative_t i=0; i<HT_Nodes_Array_Size; i++){
      HT_Nodes_Array[i]=*HT_Node_Pointer;
      HT_Node_Pointer=ht_next(hash_Table);
   }

   ht_destroy(hash_Table);

   // Sort HT_Nodes_Array.
   qsort(HT_Nodes_Array, HT_Nodes_Array_Size, sizeof(ht_node),
     &HT_Node_Compare);

   // Print the frequencies for each nucleotide sequence.
   for(intnative_t output_Position=0, i=0; i<HT_Nodes_Array_Size; i++){
      char nucleotide_Sequence[sequence_Length+1];
      for(intnative_t j=sequence_Length-1; j>-1; j--){
         nucleotide_Sequence[j]=nucleotide_For_Code(HT_Nodes_Array[i].key);
         HT_Nodes_Array[i].key>>=2;
      }
      nucleotide_Sequence[sequence_Length]='\0';

      // Output the frequency for nucleotide_Sequence to output.
      output_Position+=snprintf(output+output_Position,
        MAXIMUM_OUTPUT_LENGTH-output_Position, "%s %.3f\n",
        nucleotide_Sequence, 100.0f*HT_Nodes_Array[i].val/sequences_Length);
   }

   free(HT_Nodes_Array);
}


// Generate a count for the number of times nucleotide_Sequence appears in
// sequences and then save it to output.
static void generate_Count_For_Sequence(const char * const sequences,
  const intnative_t sequences_Length, const char * const nucleotide_Sequence,
  char * const output){
   const intnative_t nucleotide_Sequence_Length=strlen(nucleotide_Sequence);

   ht_ht * hash_Table=ht_create(INITIAL_HASH_TABLE_SIZE);

   uint64_t key=0;
   for(intnative_t i=0; i<sequences_Length; i++){
      const uint64_t mask=((uint64_t)1<<2*nucleotide_Sequence_Length)-1;
      key=(key<<2 & mask) | sequences[i];
      if(i>=nucleotide_Sequence_Length){
         ht_find_new(hash_Table, key)->val++;
         check_Hash_Table_Density_And_Grow_If_Necessary(&hash_Table);
      }
   }

   // Generate key for the sequence.
   key=0;
   for(intnative_t i=0; i<nucleotide_Sequence_Length; i++)
      key=(key<<2) | code_For_Nucleotide(nucleotide_Sequence[i]);

   // Output the count for nucleotide_Sequence to output.
   intnative_t count=ht_find(hash_Table, key)->val;
   snprintf(output, MAXIMUM_OUTPUT_LENGTH, "%jd\t%s", (intmax_t)count,
     nucleotide_Sequence);

   ht_destroy(hash_Table);
}


int main(){
   char buffer[4096];

   // Find the start of the third nucleotide sequence.
   while(fgets(buffer, sizeof(buffer), stdin) && memcmp(">THREE", buffer,
     sizeof(">THREE")-1));

   // Start with 1 MB of storage for reading in the nucleotide sequence and
   // grow exponentially.
   intnative_t nucleotide_Sequence_Capacity=1048576;
   intnative_t nucleotide_Sequence_Size=0;
   char * nucleotide_Sequence=malloc(nucleotide_Sequence_Capacity);

   // Start reading and encoding the third nucleotide sequence.
   while(fgets(buffer, sizeof(buffer), stdin) && buffer[0]!='>'){
      for(intnative_t i=0; buffer[i]!='\0'; i++){
         if(buffer[i]!='\n')
            nucleotide_Sequence[nucleotide_Sequence_Size++]=
              code_For_Nucleotide(buffer[i]);
      }

      // Make sure we still have enough memory allocated for any potential
      // nucleotides in the next line.
      if(nucleotide_Sequence_Capacity-nucleotide_Sequence_Size <
        sizeof(buffer)){
         nucleotide_Sequence_Capacity*=2;
         nucleotide_Sequence=realloc(nucleotide_Sequence,
           nucleotide_Sequence_Capacity);
      }
   }

   // Free up any leftover memory.
   nucleotide_Sequence=realloc(nucleotide_Sequence, nucleotide_Sequence_Size);

   char output_Buffer[7][MAXIMUM_OUTPUT_LENGTH];

   // Do the following functions in parallel.
   #pragma omp parallel sections
   {
      #pragma omp section
      { generate_Frequencies_For_Sequences(nucleotide_Sequence,
        nucleotide_Sequence_Size, 1, output_Buffer[0]); }
      #pragma omp section
      { generate_Frequencies_For_Sequences(nucleotide_Sequence,
        nucleotide_Sequence_Size, 2, output_Buffer[1]); }

      #pragma omp section
      { generate_Count_For_Sequence(nucleotide_Sequence,
        nucleotide_Sequence_Size, "GGT", output_Buffer[2]); }
      #pragma omp section
      { generate_Count_For_Sequence(nucleotide_Sequence,
        nucleotide_Sequence_Size, "GGTA", output_Buffer[3]); }
      #pragma omp section
      { generate_Count_For_Sequence(nucleotide_Sequence,
        nucleotide_Sequence_Size, "GGTATT", output_Buffer[4]); }
      #pragma omp section
      { generate_Count_For_Sequence(nucleotide_Sequence,
        nucleotide_Sequence_Size, "GGTATTTTAATT", output_Buffer[5]); }
      #pragma omp section
      { generate_Count_For_Sequence(nucleotide_Sequence,
        nucleotide_Sequence_Size, "GGTATTTTAATTTATAGT", output_Buffer[6]); }
   }

   for(intnative_t i=0; i<7; printf("%s\n", output_Buffer[i++]));

   free(nucleotide_Sequence);

   return 0;
}
