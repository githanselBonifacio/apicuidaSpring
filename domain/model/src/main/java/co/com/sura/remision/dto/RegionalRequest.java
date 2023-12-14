package co.com.sura.remision.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class RegionalRequest {
    @JsonProperty("idCiudad")
    private String idRegional;
    private String nombre;
    private String codigoDANE;
    private String codigoIPS;
}
