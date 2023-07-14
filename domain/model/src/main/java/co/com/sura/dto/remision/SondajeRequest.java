package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class SondajeRequest {
    private String idSondaje;
    private String tipoSondaje;
    private String idTipoSondaje;
    private String sondaje;
    private Integer totalSesiones;
    private String tipoPrestacion;
}
