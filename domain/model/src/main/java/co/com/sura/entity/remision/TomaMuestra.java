package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TomaMuestra {
    private String tipoMuestra;
    private boolean requiereAyuno;
    private String tipoPrestacion;
}
