package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TipoCuracionRequest {
    private String idTipoCuracion;
    private String descripcion;
}
