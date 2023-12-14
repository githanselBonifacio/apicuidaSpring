package co.com.sura.mapbox.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class Route {
    @JsonProperty("weight_typical")
    private double weightTypical;

    @JsonProperty("duration_typical")
    private double durationTypical;

    @JsonProperty("weight_name")
    private String weightName;
    private double weight;
    private double duration;
    private double distance;
    private List<Leg> legs;
    private String geometry;

}
