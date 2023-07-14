package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CuracionRequest {
    private String descripcion;
    private TipoCuracionRequest tipoCuracion;
    private List<DiaRequest> dias;
    private Integer sesiones;
}
