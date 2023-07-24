package co.com.sura.response.mapbox;

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
public class Leg {
    @JsonProperty("via_waypoints")
    private List<Object> viaWaypoints;
    private List<Object> admins;
    @JsonProperty("weight_typical")
    private double weightTypical;
    @JsonProperty("duration_typical")
    private double durationTypical;
    private double weight;
    private double duration;
    private List<Object> steps;
    private double distance;
    private String summary;
}
