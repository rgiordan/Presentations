#!/usr/bin/env bash

DATE=$(date +"%Y%m%d")

cp research_statement/research_statement.pdf submittable/research_$DATE.pdf
cp resume/ryan_giordano_academic_cv.pdf submittable/cv_$DATE.pdf
cp teaching_statement/teaching_statement.pdf submittable/teaching_$DATE.pdf
cp diversity_statement/diversity_statement.pdf submittable/diversity_$DATE.pdf
