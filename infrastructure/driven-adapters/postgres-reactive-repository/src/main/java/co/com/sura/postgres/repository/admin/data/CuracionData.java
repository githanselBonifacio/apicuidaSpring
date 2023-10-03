package co.com.sura.postgres.repository.admin.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "curaciones")
public class CuracionData {
    @Id
    @Column("id_curacion")
    private Integer idCuracion;

    @Column ("tipo_curacion")
    private String tipoCuracion;

    private String descripcion;

    private Integer sesiones;

    @Column("id_cita")
    private String idCita;
}
