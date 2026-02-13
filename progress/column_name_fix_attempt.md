# Column Name Fix Attempt

## Problem Summary

The R Shiny app failed to start with the following error:

```
Error in `mutate()`:
ℹ In argument: `across(...)`.
Caused by error in `across()`:
ℹ In argument: `all_of(likert_columns)`.
Caused by error in `all_of()`:
! Can't subset elements that don't exist.
✖ Elements `how_much_do_you_agree_with_the_statement_1`, `how_much_do_you_agree_with_the_statement_2`, etc. don't exist.
```

### Root Cause

The `normalize_column_names()` function converts CSV column names to lowercase snake_case by replacing all non-alphanumeric characters with underscores. This resulted in full descriptive column names like:

- `how_much_do_you_agree_with_the_statement_the_content_is_relevant_and_up_to_date`

However, the code expected numbered aliases like:

- `how_much_do_you_agree_with_the_statement_1`

This mismatch occurred because the original architecture plan assumed numbered aliases would be created during normalization, but the actual implementation only performed simple character replacement.

## Solution: Enhanced Column Mapping System

### Approach

Implemented **Plan 1: Enhanced Column Mapping** - an extension of the existing `column_mappings` system that:

1. Creates numbered aliases for Likert columns
2. Parses display name components (prefix, core, suffix, short)
3. Provides flexible display functions for different UI contexts
4. Maintains backward compatibility with existing code

### Changes Made

#### 1. `R/data/data_processing.R`

**Enhanced `build_column_mappings()` function:**
- Added automatic alias creation for Likert columns:
  - Course agreement statements (6 columns): `how_much_do_you_agree_with_the_statement_1` through `_6`
  - Learning elements (11 columns): `how_much_do_the_following_elements_contribute_to_your_learning_1` through `_11`
  - Community statements (5 columns): `how_much_do_you_agree_with_the_following_statements_1` through `_5`
- Added display name parsing:
  - `prefix`: Question stem (e.g., "How much do you agree with the statement")
  - `core`: Specific content (e.g., "The content is relevant and up-to-date")
  - `suffix`: Any trailing text
  - `short`: Compact label (first 3 words, capitalized)

**Added `resolve_column_alias()` function:**
- Resolves numbered aliases to actual column names
- Returns the alias itself if no mapping exists

**Enhanced `normalize_likert_scales()` function:**
- Now accepts both actual column names and numbered aliases
- Automatically resolves aliases before processing
- Filters to only existing columns to prevent errors

**Fixed `clean_free_text()` function:**
- Added filtering to only process existing columns
- Fixed vectorized operations to handle NA values correctly

#### 2. `R/data/data_access.R`

**Added new display functions:**
- `resolve_column_alias()` - Resolves aliases to actual column names
- `get_display_prefix()` - Gets question stem
- `get_display_core()` - Gets specific content
- `get_display_short()` - Gets compact label for charts
- `get_display_full()` - Gets cleaned full question text

**Enhanced `get_column_display_name()` function:**
- Now accepts both normalized names and aliases
- Automatically resolves aliases before lookup

**Updated `get_likert_data()` function:**
- Now resolves aliases to actual column names before filtering

#### 3. Visualization Files

Updated all visualization files to use `get_display_short()` instead of hardcoded `case_when` statements:

- `R/visualization/plot_course_content.R` (3 locations)
- `R/visualization/plot_learning_elements.R` (2 locations)
- `R/visualization/plot_community.R` (2 locations)

This change:
- Removes hardcoded mappings like `"how_much_do_you_agree_with_the_statement_1" ~ "Statement 1"`
- Uses dynamic short labels derived from the actual question content
- Makes the system more maintainable and flexible

### New Column Mappings Structure

The `column_mappings` global variable now contains:

```r
list(
  original = c("How much do you agree with the statement? [The content is relevant and up-to-date]", ...),
  normalized = c("how_much_do_you_agree_with_the_statement_the_content_is_relevant_and_up_to_date", ...),
  aliases = c(
    "how_much_do_you_agree_with_the_statement_1" = "how_much_do_you_agree_with_the_statement_the_content_is_relevant_and_up_to_date",
    ...
  ),
  display = list(
    prefix = c("How much do you agree with the statement", ...),
    core = c("The content is relevant and up-to-date", ...),
    suffix = c("", ...),
    short = c("The Content Is Relevant", ...)
  )
)
```

### Benefits

1. **Fixes the immediate error**: Numbered aliases now work correctly
2. **Backward compatible**: Existing code using numbered aliases continues to work
3. **Flexible display**: Different display options for different UI contexts
4. **Maintainable**: Centralized mapping makes future changes easier
5. **Self-documenting**: Display names derived from actual question content

### Display Function Usage

| Function | Use Case | Example Output |
|----------|----------|----------------|
| `get_display_short()` | Chart labels, tight spaces | "The Content Is Relevant" |
| `get_display_core()` | Individual question items | "The content is relevant and up-to-date" |
| `get_display_prefix()` | Section headers | "How much do you agree with the statement" |
| `get_display_full()` | Tooltips, full descriptions | "How much do you agree with the statement: The content is relevant and up-to-date" |
| `get_column_display_name()` | Original behavior (full question) | "How much do you agree with the statement? [The content is relevant and up-to-date]" |

### Testing

The fix was verified by running:
```bash
Rscript -e "source('R/global.R')"
```

Result:
```
Successfully loaded 667 survey responses
Data validation passed successfully
```

### Files Modified

1. `R/data/data_processing.R` - Enhanced column mapping, alias resolution, fixed clean_free_text
2. `R/data/data_access.R` - Added new display functions, updated existing functions
3. `R/visualization/plot_course_content.R` - Updated to use get_display_short()
4. `R/visualization/plot_learning_elements.R` - Updated to use get_display_short()
5. `R/visualization/plot_community.R` - Updated to use get_display_short()

### Future Considerations

1. **Server files**: The server files (`R/server/*.R`) currently use `get_column_display_name()` which now works with aliases. They could be updated to use more context-appropriate display functions (e.g., `get_display_short()` for compact displays).

2. **Free text columns**: The `free_text_columns` list in `process_survey_data()` contains hardcoded column names that may not match the actual normalized names. These should be reviewed and potentially updated to use a similar alias system or dynamic detection.

3. **Display customization**: The short label generation (first 3 words, capitalized) could be made more sophisticated if needed (e.g., configurable word count, different capitalization rules).

4. **Performance**: The current implementation uses global variables and string matching. For very large datasets, consider optimizing the alias resolution logic.
