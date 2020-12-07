# peekds

This is the "peek data standard" package, which facilitates writing scripts that convert arbitrary 2 alternative word recognition eye-tracking datasets to the `peekds` format, which allows import in [Peekbank](http://peekbank.stanford.edu). 

# Schema Column Documentation

https://docs.google.com/spreadsheets/d/1Z24n9vfrAmEk6_HpoTSh58LnkmSGmNJagguRpI1jwdo/edit?usp=sharing

# Schema 

|field_name  |field_class  |
|:-----------|:------------|
|aoi_data_id |IntegerField |
|aoi         |CharField    |
|subject     |ForeignKey   |
|t           |IntegerField |
|trial       |ForeignKey   |


|field_name    |field_class  |
|:-------------|:------------|
|aoi_region_id |IntegerField |
|l_x_max       |IntegerField |
|l_x_min       |IntegerField |
|l_y_max       |IntegerField |
|l_y_min       |IntegerField |
|r_x_max       |IntegerField |
|r_x_min       |IntegerField |
|r_y_max       |IntegerField |
|r_y_min       |IntegerField |


|field_name     |field_class  |
|:--------------|:------------|
|dataset_id     |IntegerField |
|monitor_size_x |IntegerField |
|monitor_size_y |IntegerField |
|sample_rate    |IntegerField |
|tracker        |IntegerField |
|lab_dataset_id |CharField    |


|field_name     |field_class  |
|:--------------|:------------|
|subject_id     |IntegerField |
|age            |IntegerField |
|sex            |CharField    |
|lab_subject_id |CharField    |


|field_name              |field_class  |
|:-----------------------|:------------|
|trial_id                |IntegerField |
|aoi_region              |IntegerField |
|dataset                 |ForeignKey   |
|lab_trial_id            |CharField    |
|distractor_image        |CharField    |
|distractor_label        |CharField    |
|full_phrase             |IntegerField |
|point_of_disambiguation |IntegerField |
|target_image            |CharField    |
|target_label            |CharField    |
|target_side             |CharField    |


|field_name |field_class  |
|:----------|:------------|
|xy_data_id |IntegerField |
|subject    |ForeignKey   |
|trial      |ForeignKey   |
|x          |IntegerField |
|y          |IntegerField |


|field_name |field_class  |
|:----------|:------------|
|admin_id   |IntegerField |
|date       |DateField    |
|version    |CharField    |
