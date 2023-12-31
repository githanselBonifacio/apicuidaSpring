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
public class DireccionResponse {
    private List<Route> routes;
    private List<Waypoint> waypoints;
    private String code;
    private String uuid;
}
