package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TomaMuestraRequest {
    private TipoMuestra tipoMuestra;
    private boolean requiereAyuno;
    private String tipoPrestacion;
}
