**To create your own sample groups to use in analyses, upload a comma-separated value file ( CSV file ) in the form shown in this example, but with commas separating the values. The CSV file must be a plain text file.**


<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Sample </th>
   <th style="text-align:left;"> Alternate Cancer Subtyping </th>
   <th style="text-align:left;"> Gender </th>
   <th style="text-align:left;"> Molecular Alteration </th>
   <th style="text-align:left;"> Predicted Immunotherapy Outcome </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TCGA-01-0639 </td>
   <td style="text-align:left;"> group_a </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> activating </td>
   <td style="text-align:left;"> Non-responder </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0007 </td>
   <td style="text-align:left;"> group_a </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> repressing </td>
   <td style="text-align:left;"> Responder </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCGA-02-0011 </td>
   <td style="text-align:left;"> group_b </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Partial responder </td>
  </tr>
</tbody>
</table>

## Sample column
One column must be named Sample. This column will contain the identifiers for the samples to include.

#### TCGA
The Sample column needs to contain (12-digit) TCGA participant barcode IDs. (ie. 'TCGA-02-0011')

#### PCAWG
The sample column need to contain the donor ID. (ie. 'DO1328')

## Other columns
Other columns (one or more) contain sample groupings that you can later select for analysis. For example in the above, the third column contains gender, and the values in that column can thus be used to compare immune response in cancer in women to that in men.

## Rows
Rows can be supplied for any subset of TCGA or PCAWG samples. Rows will be filtered out if they have an `NA` value for the selected group. For example, if `Molecular Alteration` is selected, the bottom row will be removed.

## Example files
[Example1](https://raw.githubusercontent.com/CRI-iAtlas/iatlas-app/main/inst/examples/example_user_group.csv)

[Example2](https://raw.githubusercontent.com/CRI-iAtlas/iatlas-app/main/inst/examples/example_user_group2.csv)
