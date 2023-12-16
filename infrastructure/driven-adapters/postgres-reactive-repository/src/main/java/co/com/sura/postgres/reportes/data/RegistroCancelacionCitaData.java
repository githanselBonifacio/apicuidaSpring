package co.com.sura.postgres.reportes.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.Id;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "registro_cancelacion_citas")
public class RegistroCancelacionCitaData {
    @Id
    private String id;

    @Column("fecha_cancelacion")
    private LocalDate fechaTurno;

    @Column("id_cita")
    private String idCita;

    @Column("id_motivo_cancelacion")
    private Integer idMotivoCancelacion;
}
