package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class MunicipioRequest {
    private String idMunicipio;
    private String idRegional;
    private String nombre;
    private long latitud;
    private long longitud;
}
