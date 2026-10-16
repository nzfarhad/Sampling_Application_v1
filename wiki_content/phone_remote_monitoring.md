# Phone and Remote Monitoring Sampling

## Overview

Phone surveys reach beneficiaries in areas field teams cannot visit, at lower cost. They also miss people without a phone, and reach some groups (often women) less well. This page covers sampling from beneficiary lists, call rules and how to report coverage.

## The Frame: Beneficiary Lists

Most monitoring phone surveys sample from the implementing partner's beneficiary list. Check before sampling:

- **Share with a usable number:** what percentage of beneficiaries have a phone number on the list, by province and by sex of the registered beneficiary. Households without a number cannot be sampled; this is the coverage gap.
- **Whose number it is:** often the male household head's phone, or a neighbour's or community leader's. Record this, because it affects who can be interviewed.
- **Duplicates:** one number listed for several households (shared phones, or a leader's phone). Decide in advance whether to keep one record per number.
- **Data protection:** phone numbers are personal identifiers. Do not upload them to this app. Upload an anonymised list with an ID, draw the sample, and match IDs back to numbers offline.

## Sample Size: Allow for Unreachable Numbers

Phone surveys lose more of the sample than face-to-face surveys: wrong numbers, numbers switched off, no answer, refusals. Size the sample for the completed interviews you need:

**Numbers to draw = completed interviews needed / (contact rate × cooperation rate)**

**Example:** 400 completed interviews are needed. A previous round reached 60% of numbers, and 85% of those reached agreed. Draw 400 / (0.60 × 0.85) = 784.3, rounded up to **785 numbers**. In the Sample Generator, draw the 400 as the main sample and the rest as a buffer (385 more, a buffer of about 97%), and release reserves in batches in `reserve_order`.

## Call Protocol

Fix the rules before fieldwork and apply them to every number:

- **Attempts:** make a set number of attempts, for example 3 to 5, on **different days and times** (including evenings and a weekend day), before a number is classed as non-contact.
- **Outcome codes:** record the result of every attempt: completed, partial, refused, wrong number, no answer, switched off or unreachable, ineligible, call back later.
- **Appointments:** honour call-back requests, and count them as attempts.
- **Replacement:** replace a number only after the attempt limit or a final outcome (refusal, wrong number). Take the next reserve in `reserve_order` within the same stratum, not a number the enumerator chooses. Log it in the Replacement Log.
- **Monitoring:** track contact and completion rates daily by stratum and by enumerator. Low rates for one enumerator usually mean a protocol problem.

## Coverage Bias

A phone sample describes **beneficiaries with a reachable phone number**, not all beneficiaries. To judge and reduce the bias:

- Compare people with and without a number on the list (province, sex of head, household size, assistance type). Report the differences.
- If the list holds those characteristics, **post-stratify**: adjust weights so the completed sample matches the full list on them.
- State the coverage gap in the report, e.g. "18% of listed beneficiaries had no phone number and could not be sampled".
- Random digit dialing (RDD) is not a substitute in Afghanistan. There is no public list of allocated number ranges, and phone ownership is lower among women and in rural areas.

## Reaching Female Respondents

- When women are the target respondents, sample **women listed as beneficiaries** where the list allows it, and ask for the respondent by name.
- When the number belongs to a male relative, explain the purpose, ask to speak to the listed woman, and offer a time that suits her. Do not pressure; record a refusal by the gatekeeper as its own outcome code.
- Use **female enumerators** for female respondents.
- Keep questions on sensitive topics short, and allow the respondent to stop if someone is listening.
- Report completion rates for women and men separately. A low rate for women is a finding in itself.

## Reporting

- The frame (source, date, number of records, share with a usable number).
- Numbers drawn, attempts per number, and final outcome counts.
- Response rate: completed interviews divided by eligible numbers drawn (state which outcomes count as eligible).
- Coverage gaps and any weighting applied.

## In This Tool

- **Sample Generator:** draw the main sample plus a large buffer, stratified by province or sex of the beneficiary. Use `reserve_order` for replacements, and use the Replacement Log sheet to record them.
- **Sample Calculator:** use the non-response rate to allow for both non-contact and refusal (e.g. 49% non-response in the example above).
