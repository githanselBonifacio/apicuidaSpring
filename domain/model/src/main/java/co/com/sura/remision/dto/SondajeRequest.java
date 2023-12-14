package co.com.sura.remision.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class SondajeRequest {
    @JsonIgnore
    private String idSondaje;
    private String tipoSondaje;
    @JsonIgnore
    private String idTipoSondaje;
    private String sondaje;
    private Integer totalSesiones;
    private String tipoPrestacion;
}
