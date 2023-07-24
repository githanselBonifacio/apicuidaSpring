package co.com.sura.mapbox.repository.map.config;


import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Data
@Component
@ConfigurationProperties(prefix = "mapbox")
public class MapboxProperties {

    private String accessToken;
    private String apiUrlBase;
}
