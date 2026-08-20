# Arctic Rivers

***Investigating the 2020 Taymyr Peninsula permafrost thaw slump event and its aftermath***

This work uses satellite remote sensing to identify permafrost thaw slump occurrence, track progressive growth of thaw slumps, and tie thaw slump erosion to increased river sediment transport.

<p align="center">
<img src="images/f01_overview_map_and_slump_progression_data_and_example.png" width="600">
</p>

The focus is a regional thaw slump event on the Taymyr Peninsula, Siberia, Russia. Hundreds of thaw slumps appear to have initiated in a single week, during the peak warmth of an extended heat wave in 2020. Since, these thaw slumps have continued to expand rapidly.

<p align="center">
<img src="images/f05_taymyr_climate_combined_plot.png" width="600">
</p>

Rivers have responded accordingly, with increased suspended sediment concentration (SSC) measurable using satellite-based methods. Rivers initially were the "canary in the coal mine", registering a stepwise increase in SSC in early August, 2020, when slumps initiated. SSC for these rivers has continued to increase as slumps have grown since 2020.

<p align="center">
<img src="images/f02_taymyr_SSC_timeseries_by_site.png" width="600">
</p>

This work is in review at *The Cryosphere*, with a [pre-print available](https://doi.org/10.5194/egusphere-2025-5691) ([Dethier et al., 2025](#ref-dethier2025)). The approach builds on existing methods for satellite-based estimation of SSC, extending them to Sentinel-2 data ([Dethier et al., 2020](#ref-dethier2020); [Dethier et al., 2022](#ref-dethier2022)). This allows the tracking of rapid slump failure on a near-daily basis in 2020.

We automated slump identification and delineation using conventional and machine learning methods for Landsat and Sentinel-2 data in [Google Earth Engine](https://earthengine.google.com/).

<p align="center">
<img src="images/Taymyr_slumps_outlined_example_NIR.png" width="600">
</p>

Slumps are outlined in white in the false color image (RGB: near-infrared, red, green) from 2024, above. Some small and incipient slumps are not yet identified. The same image is shown in true color below.

<p align="center">
<img src="images/Taymyr_slumps_outlined_example.png" width="600">
</p>

## References

<a id="ref-dethier2020"></a>
Dethier, E. N., Renshaw, C. E., & Magilligan, F. J. (2020). Toward improved accuracy of remote sensing approaches for quantifying suspended sediment: Implications for suspended-sediment monitoring. *Journal of Geophysical Research: Earth Surface*, 125(7), e2019JF005033. https://doi.org/10.1029/2019JF005033

<a id="ref-dethier2022"></a>
Dethier, E. N., Renshaw, C. E., & Magilligan, F. J. (2022). Rapid changes to global river suspended sediment flux by humans. *Science*, 376(6600), 1447–1452. https://doi.org/10.1126/science.abn7980

<a id="ref-dethier2025"></a>
Dethier, E. N., Erikson, C. M., & Renshaw, C. E. (2025). Thaw slump erosion accelerates fluvial sediment transport after a heatwave on the Taymyr Peninsula, Russia. *EGUsphere* [preprint], 1–35. https://doi.org/10.5194/egusphere-2025-5691
